""" Preprocess SCIPY CODATA into a fortran module.
https://docs.scipy.org/doc/scipy/reference/constants.html

TODO(Alex) Add mathematical constants
Math constants: pi, golden, golden_ratio
Note, there are more math constants in the python source than listed here:
https://docs.scipy.org/doc/scipy/reference/constants.html

Duplicates with descriptive names:
speed_of_light, Planck, gravitational_constant, elementary_charge, gas_constant, fine_structure
Avogadro, Boltzmann, Stefan_Boltzmann, electron_mass, proton_mass, neutron_mass

"""
import re
from typing import List

from scipy.constants import physical_constants as conversions
from scipy.constants import c, mu_0, epsilon_0, h, hbar, G, g, e, R, \
    alpha, N_A, k, sigma, Wien, Rydberg, m_e, m_p, m_n


class ModuleStartEnd:
    def __init__(self, start, end):
        self.start = start
        self.end = end


# One indent
indent = "   "


def set_module_template(module_name: str, use_list: List[str]) -> ModuleStartEnd:
    """ Define the boilerplate for the start and end of a fortran module.

    ModuleStartEnd.start =
    module module_name
        use another_module, only: dp
        implicit none
        private
    ...
    ModuleStartEnd.end =
    end module module_name

    :param module_name: Module name
    :param use_list: List of `use module, only:`
    :return: Start and end of a module.
    """
    source = f'module {module_name}\n'
    for module in use_list:
        source += indent + module + '\n'
    source.rstrip('\n')
    source += indent + "implicit none\n" + indent + "private\n\n"

    return ModuleStartEnd(source, f"\nend module {module_name}\n")


def fortran_constants(module_template: ModuleStartEnd) -> str:
    """ Convert physical constants to fortran source.

     Not sure why API for constants has not been provided. Not be confused
     with conversions, which is `physical_constants` dict (poor choice of name).

    :param module_template: Start and end strings of the f90 module
    :return: Fortran-formatted string. To be written as a .f90 file.
    """
    constants = {'speed_of_light': (c, "Speed of light in vacuum"),
                 'mu_0': (mu_0, "The magnetic constant"),
                 'epsilon_0': (epsilon_0, "The electric constant (vacuum permittivity)"),
                 'h_planck': (h, "The Planck constant, h"),
                 'hbar': (hbar, "h / 2pi"),
                 'gravitational_constant': (G, "Newtonian constant of gravitation"),
                 'g_due_to_gravity': (g, "Standard acceleration of gravity"),
                 'e': (e, "Elementary charge, e"),
                 'R_molar_gas_constant': (R, "Molar gas constant"),
                 'alpha_fine_structure_constant': (alpha, "Fine-structure constant"),
                 'N_avogadro': (N_A, "Avogadro constant"),
                 'k_boltzmann': (k, "Boltzmann constant"),
                 'sigma': (sigma, "Stefan-Boltzmann constant"),
                 'wien': (Wien, "Wien displacement law constant"),
                 'rydberg': (Rydberg, "Rydberg constant"),
                 'm_electron': (m_e, "Electron mass"),
                 'm_proton': (m_p, "Proton mass"),
                 'm_neutron': (m_n, "Neutron mass")
                 }

    source = f"""!> CODATA 2018 Physical constants, sourced from SCIPY
!> https://docs.scipy.org/doc/scipy/reference/constants.html
"""
    source += module_template.start

    for name, entry in constants.items():
        value, description = entry
        documentation_line = indent + f"!> CODATA 2018. {description}"
        data_line = indent + f"real(dp), public, parameter :: {name} = {value}_dp"
        source += documentation_line + '\n' + data_line + '\n\n'

    source += module_template.end
    return source


def fortran_conversions(module_template: ModuleStartEnd) -> str:
    """ Convert scipy.constants to a fortran source.

    Assumes double precision defined in `precision` module
    as dp.

    :param module_template: Start and end strings of the f90 module
    :return: Fortran-formatted string of physical constants/conversions.
    """
    constants = conversions.copy()
    # No simple conversion for this key, nor is it at all needed
    del constants["{220} lattice spacing of silicon"]

    source = f"""!> CODATA 2018 Conversion factors, sourced from SCIPY
!> https://docs.scipy.org/doc/scipy/reference/constants.html
"""
    source += module_template.start

    for key, entry in constants.items():
        value, unit, uncertainty = entry
        name = "_".join(x for x in key.split())
        # Symbols to remove or convert
        name = re.sub('[.,]', '', name)
        name = re.sub('[/]', '_over_', name)
        name = re.sub('[()]', '', name)
        name = re.sub('[-]', '_', name)
        name = name.lower()
        documentation_line = indent + f"!> CODATA 2018. {key} with unit {unit}"
        data_line = indent + f"real(dp), public, parameter :: {name} = {value}_dp"
        source += documentation_line + '\n' + data_line + '\n\n'

    source += module_template.end
    return source


if __name__ == "__main__":

    # Physical constants
    module_name = "physical_constants"
    template = set_module_template(module_name, ["use precision, only: dp"])
    file_string = fortran_constants(template)
    with open(module_name + '.f90', mode='w') as fid:
        fid.write(file_string)

    # Conversions
    module_name = "conversions"
    template = set_module_template(module_name, ["use precision, only: dp"])
    file_string = fortran_conversions(template)
    with open(module_name + '.f90', mode='w') as fid:
        fid.write(file_string)
