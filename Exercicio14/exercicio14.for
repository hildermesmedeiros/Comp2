      implicit none
      real*8,A(50,30),B(30),C(50)
      integer i,j,m,n
      
      write(*,*)'Programa matriz x vetor'
      write(*,*)'turma de compII'
      write(*,*)'Autor: Hildermes Jose Medeiros Filho'
      write(*,*)'13/07/2015'
      
c     Lendo m e n
      m=-1
      while(m.LE.0.or.m.GT.50)do
       write(*,*)'Entre com m:'
       read(*,*)m
      end while
      n=-1
      while(n.LE.0.or.n.GT.30)do
       write(*,*)'Entre com n:'
       read(*,*)n
      end while
c     Lendo a matriz A
      Do i=1,m
       Do j=1,n
        write(*,*)'Entre com A(',i,',',j,')'
        read(*,*) A(i,j)
       End Do
      End Do
c     Lendo a matriz B
      Do i=1,n
       write(*,*)'Entre com B(',i,')'
       read(*,*)B(i)
      End Do       
c     Calculando C
      Do i=1,m
       C(i)=0
       Do j=1,n
        C(i)=C(i)+ A(i,j)*B(j)
       End Do
      End Do
c     Resultado
      write(*,*)'Matriz A'
      Do i=1,m
      write(*,30)(A(i,j),j=1,n)
      End DO
 30   Format(50F13.3)
 
      write(*,*)
      write(*,*)'Matriz B'
      Do i=1,m
       write(*,40)B(i)
      End Do  
 40   Format(50F13.3)       
  
      write(*,*)
      
      write(*,*)'O vetor C=A*B eh:'      
      Do i=1,m
       write(*,50)C(i)
      End Do 
 50   Format(50F13.3)      
      write(*,*)
      Write(*,*)'Aperte uma tecla para fechar'
      read(*,*)
      End                 
           