# c=/opt/gcc-trunk/bin/gfortran -fimplicit-none -fbounds-check
# c=gfortran -fimplicit-none  -fcoarray=single -fbounds-check -g3 -fdefault-real-8 -Wextra -Wunused
c=gfortran -fimplicit-none  -fcoarray=single -fbounds-check -g3 -fdefault-real-8 -O3 -finit-real=nan
#    c=gfortran -fimplicit-none  -fcoarray=single -fbounds-check -g -fbacktrace
# c=gfortran -fimplicit-none  -fcoarray=single -O3
# c=g95 -g -fbounds-check  -O0 -fimplicit-none  -fintrinsic-extensions -ftr15581 -ftrace=full
#   c=g95  -ftrace=full  -g -fbounds-check  -O3 -funroll-loops -ftr15581 -fimplicit-none
# c=g95   -O3 -ftr15581 -fintrinsic-extensions -fimplicit-none
#     c=g95  -O3 -funroll-loops -fimplicit-none  -ftr15581
#  c=ifort -O3 -fp-model precise -ftz -funroll-loops -coarray -coarray-num-images=1 -implicitnone -debug full
#   c=ifort -O0 -coarray -coarray-num-images=1 -implicitnone -CB -g
#   c=ifort -O0  -implicitnone -CB -g
# c=g95 -std=F -O3 -funroll-loops -ftr15581
# c=/opt/intel/fc/10.1.018/bin/ifort  -O3
#  c=ifort -O3
#c=gfortran -g

d=ade2d-`date -I`
t=ade2d-`date +%Y-%m-%d-%H-%M-%S`

all : main.o types.o glob.o tools.o
	$c -g -o ade2d main.o types.o glob.o tools.o



#-------begin CORE_obj--------------------------------
types.o: types.f90
	$c -c types.f90 
glob.o: types.o glob.f90
	$c -c glob.f90
solve.o: types.o glob.o solve.f90
	$c -c solve.f90
tools.o: types.o glob.o solve.o tools.f90
	$c -c tools.f90
main.o: types.o glob.o tools.o main.f90
	$c -c main.f90
#---------end CORE_obj------------------------------


clean:
	rm -rf *.o *.mod 

tar :
	 tar -czf $d.tgz src Makefile drutes.conf 

tartime: 
	tar -czf $t.tgz src Makefile drutes.conf out

