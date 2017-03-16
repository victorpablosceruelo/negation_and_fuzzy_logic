echo off
delete parametrod
echo [preT]. >  parametros
echo generateP("%1"). >> parametros
echo halt. >> parametros
xsb < parametros
del paramentros
echo [metaAb]. >  parametros
echo [%1]. >  parametros
xsb < parametros