FF90=gfortran
USR=/usr

read_data.o:
	$(FF90) -c read_data.f90
prueba: read_data.o
	$(FF90) -o prueba read_data_probe.f90 read_data.o
clean:
	rm *.o *.mod prueba
