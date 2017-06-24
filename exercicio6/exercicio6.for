      Implicit None
      Integer N,I
      Real *8 X(100), Media, Desvio_Padrao, Soma, Soma2 
      
      Write(*,*)'Programa: Media e Desvio Padrao'
      write(*,*)'Data:13/07/2015'
      write(*,*)'Autor: Hildermes Jose Medeiros Filho'
c     Ler N 
      N=-1
       While(n.LE.0.OR.N.GT.100) DO
        Write(*,*) 'Entre com N' 
        Read(*,*) N
       End While
       
        Soma=0.d0 
        DO I= 1,N
         Write(*,10) I
10       Format (' Entre com X(',I3,'):',$)
         Read(*,*) X(I)         
         Soma=Soma+x(i) 
        END DO      
c       Calculo da Media        
          Media= Soma/DBLE(N)        
c       Calculo do Desvio Padrao            
          Soma2=0.D0
           DO I= 1,N 
           Soma2= Soma2+(X(I)-MEDIA)*(X(I)-MEDIA)
           END DO       
           IF(N.GT.1) THEN
              Desvio_Padrao= DSQRT(SOMA2/DBLE(N-1))
              WRITE(*,*) 'MEDIA= ',MEDIA, ' DESVIO_PADRAO= ',DESVIO_PADR
     &AO 
              ELSE
              write(*,*)
              WRITE(*,*) 'MEDIA= ',MEDIA, ' DESVIO_PADRAO= ',DESVIO_PAD
     &RAO
           END IF
               write(*,*)
               WRITE(*,*) ' TECLE ENTER'
               READ (*,*) 
               
      END             
