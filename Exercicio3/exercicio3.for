C     Programa: Fatorial
      IMPLICIT NONE
      Real*8 F
      Integer n,i


      Write(*,*)'Programa Fatorial'
      write(*,*)'Data:13/07/2015'
      write(*,*)'Autor: Hildermes Jose Medeiros Filho'
C     LEITURA DE n      
      n=-1
      WHILE (n.LT.0) DO 
        WRITE(*,*)'Entre com o numero de termos, que seja positivo'
        READ(*,*) n
      END WHILE
      F=1.D0
      DO i=1,n
        F=F*dble(i)
      END DO
      WRITE(*,*)'Fatorial de ',n,' = ',F
      write(*,*)
      WRITE(*,*)'Pressione Enter'
      READ(*,*)
      END

