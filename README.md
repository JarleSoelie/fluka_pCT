# fluka_pCT
This fluka simulation was made to investigate the radiation environment inside and around the Bergen DTC during both pCT and proton therapy.
**pCT_DTC.inp** is the input file detailing all the geometries, physics, outputs, etc... Everything here is based on the GATE simulations found in the *gate_pCT* repository. 

The fortran file **pCT.f** is a fixed version of a pencil beam scanning system covering the area from -8.5cm to 8.5cm in the lateral direction and from -6cm to 6cm in the height direction. There are 2500 protons in each spot. This is based on the 7mm pencil beam from the GATE simulations<br />
The fortran file **pTherapy.f** creates a spread out bragg peak (SOBP) inside the cylindrical water phantom and covers a 5cm x 5cm x 5cm volume.<br />
