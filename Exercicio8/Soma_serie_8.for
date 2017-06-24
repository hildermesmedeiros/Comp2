c     Hildermes 2012.2.06065.11
c     implicit none, sempre primeiro comando
      IMPLICIT NONE
c     Primeiro declaracao de variaveis      
c     n, quantidade de numeros da serie
c     u, iostat para nao aceitar valores errados e nao fechar
      integer n,u,i
c     Reais de dupla precisao. X, dado de entrada. Soma, soma da serie.
      real*8 X,Soma
c     Format
      character fmt*14
c     CABEÇALHO      
      Write(*,*)'Soma serie'
      write(*,*)'a1=X/2 ; a2=-X**2/4; a3=X**3/6; a4=-X**4/8;...+an'
      write(*,*) 'Data: 30/06/2015'
      write(*,*) 'Autor: Hildermes Jose Medeiros Filho'
      write(*,*)       
c     Verificacao do valor de n            
      n=-1
      while(n.LE.0)do
       write(*,*)'Entre com n:'
       read(*,*,iostat=u)n
       if(u.gt.0)then
        write(*,*)'n, deve ser um numero inteiro.Digite novamente.'
        write(*,*)
       endif 
      end while
      write(*,*)
c     calculando
 200  if(u.GT.0)then
        write(*,*)'X1, deve ser um numero real.Digite novamente.'
      end if  
      Write(*,*)'Entre com o valor de X1: '
      read(*,*,err=200,iostat=u)X
      Soma=0.d0
      Do i=1,n
       soma=soma+((-1)**(i+1))*(x**i)/(i*2.d0)
      end do
c     escrevendo o resutado
      write(fmt,*)'(A21,F13.3)'
      write(*,fmt)'O valor da serie eh: ',soma !Para evitar o espaço bastava guardar quantos digitos inteiros tem a soma e guardar no fmt, peace of cake
      write(*,*)
      write(*,*)'Aperte uma tecla para sair'
      read(*,*)
      end      
      
            
