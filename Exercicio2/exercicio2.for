c     Primeiro comando
      Implicit None
      
      Real *8 a1, R, SOMA, AI, A(100)
      Integer N,I
     
c     Leitura de A1 , R , N
      Write(*,*)'Programa Calcula pi'
      write(*,*)'Data:13/07/2015'
      write(*,*)'Autor: Hildermes Jose Medeiros Filho'
      Write (*,*) ' Programa: Soma de P.A '
      Write (*,*) ' Entre com o Primeiro Termo '
      Read (*,*) a1
      Write (*,*) ' Entre com a Razao '
      Read (*,*) R
      N = -1
       
        While  ( N.LE.0) DO
         Write (*,*) ' Entre com o numero de Termos '
         Read  (*,*) N
        End While
        
          a(1) = a1
          Soma = a1
          aI = a1
          
           DO I = 2, N
            AI = AI + R
            A(i) = AI
            Soma = Soma + AI
           END DO
           
           Write (*,*) ' A Soma dos',n, ' elementos da P.A EH: ' , Soma
           Write (*,*)          
           Write (*,*) ' Pressione Enter para sair do programa ' 
           Read (*,*)
      
      END      

