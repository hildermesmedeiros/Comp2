      Implicit None
      Integer N,I,N0,NPOS,EXISTE_NEG
      Real *8 S,M,P,AI 
      
      Write(*,*)'Programa: Media Aritmetica Positiva'
      write(*,*)'Data:13/07/2015'
      write(*,*)'Autor: Hildermes Jose Medeiros Filho'
c     Ler N 
      N=-1
      EXISTE_NEG=0
      S=0.D0
      N0=0
      P=1.D0
      NPOS=0      
            
      
       While(N.LE.0.D0) DO
        Write(*,*) 'Entre com N' 
        Read(*,*) N
       End While
       
       
        DO I= 1,N
         Write(*,*) 'Entre com o termo ',I 
         Read(*,*) AI
         
         
        IF(AI.LT.0.D0) THEN 
         EXISTE_NEG=1
         P=P*AI
          ELSE
           
           IF (AI.GT.0.D0) THEN
           S=S+AI
           NPOS=NPOS+1
            ELSE
            N0=N0+1
           END IF 
        END IF   
        
        END DO
        
         
          IF (NPOS.GT.0)THEN
            M=S/DBLE(NPOS)
            Write(*,*) 'A MEDIA EH: ', M 
             ELSE
             Write(*,*) 'NAO HA VALORES POSITIVOS.'
          END IF
          
          
            IF (EXISTE_NEG.EQ.1) THEN
                 Write(*,*) 'O PRODUTO DOS NEGATIVOS EH: ', P
                 ELSE
                  Write(*,*) 'NAO EXISTEM NEGATIVOS.'
            END IF
            
             
             Write(*,*) 'A QUANTIDADE DE NUMEROS NULOS EH: ', N0
             Write(*,*)
             Write(*,*) ' PRESSIONE ENTER PARA SAIR'
             READ(*,*) 
            
      END            

