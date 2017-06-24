      Implicit None
      Integer n,i
      Real *8 S,M,AI
      Write(*,*)'Programa Media aritmetica'
      write(*,*)'Data:13/07/2015'
      write(*,*)'Autor: Hildermes Jose Medeiros Filho'
      
      
      n=-1
      S=0
c     Testando o Valor de n 

       While(n.LE.0.D0) DO
        Write(*,*) 'Entre com n' 
        Read(*,*) n
       End While
      
c     Fazendo a soma dos valores   

        DO i=1,N
         Write(*,*) 'Entre com o termo', I
         Read(*,*) AI
         S= S+AI
        End DO
        
c     Calculando a media
         M=S/DBLE(n)
          Write(*,*) 'A media aritmetica eh: ',M
          write(*,*)
          Write(*,*) 'Pressione enter para sair'
          Read(*,*)
      END

