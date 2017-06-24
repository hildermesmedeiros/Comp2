      Implicit None
      Integer N,I,K
      Real *8 A(50,50), B(50), X(50) 
      
      
      Write(*,*)'Programa: Sistema AX=B'
      write(*,*)'Data:13/07/2015'
      write(*,*)'Autor: Hildermes Jose Medeiros Filho'      
    
c     Ler N 
      N=-1   
       While(N.LE.0.OR.N.GT.50) DO
        Write(*,*) 'Entre com N' 
        Read(*,*) N
       End While
       
       
        DO I= 1, N-1
        Write(*,80) I,I,I,N
80      Format ('Entre com A(',I2',',I2') e A(',I2,',',I2,',')
        Read(*,*) A(I,I), A(I,N) 
        END Do
        
        
         Write(*,90) N,N
90      Format('Entre com A(',I2,',',I2,')')
         Read (*,*) A(N,N)
         
         
          DO I= 1,N
           Write (*,100) I
100        Format (' Entre com B(',I2,')')
           Read (*,*) B(I)
          END DO
          
          
            X(N)= B(N)/A(N,N)
            Do K= 1,N-1
            X(K)= (B(K) - A(K,N) * X(N)) /A(K,K)
            End DO
            
            
            DO I= 1,N
            Write(*,*) 'X(',I,')= ',X(I) 
            END DO
            write(*,*)
            
            Write(*,*) 'Tecle enter para sair'
             Read(*,*)
      END
