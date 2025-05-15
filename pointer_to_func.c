#include <stdio.h>
#include <math.h>
#include <stdbool.h>

// Fictitious field definitions
float e_field(float x) {
    return x * 2.0;
}

float a_field(float x) {
    return pow(x, 2.0);
}

float b_field(float x) {
    return sin(x);
}


// Typedef for function pointers
typedef float (*FieldFunction)(float);
typedef float (*ElectricFieldFunction)(float);

int main() {

  const bool ENUM_E_FIELD_ELECTRIC = true;
  const ElectricFieldFunction electric_functions[3] = {e_field, a_field, b_field};

  FieldFunction (*field)[3];  
  if (ENUM_E_FIELD_ELECTRIC){
   // Assign the address of electric_functions to field
    field = (FieldFunction (*)[3])&electric_functions;
  }
  
  for (int i = 0; i < 3; ++i) {
    const float result = (*field)[i](2.0);
    printf("%f\n", result);
  }
  
  return 0;
}

