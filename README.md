# ABAQUS-Import-of-transient-temp-through-UTEMP
Import of thermal transient results into Abaqus by means of UTEMP subroutine

If different software is used to calculate transient temperature and mesh is same for temperature and mechanical calculations the UTEMP subroutine can be used to apply temperature in the Abaqus mechanical model.
Edit path to a result file and check the sizes of the arrays in the UTEMP.f before use the subroutine.

The result file has following structure:
- Every section begins with "Time=___"
- The first Column is node number; the second column is temperature. 

*Temp.dat is an example of a result file which UTEMP.f can read.
