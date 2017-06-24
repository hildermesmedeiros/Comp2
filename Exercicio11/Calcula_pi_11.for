c     Implicit None, primeiro comando, sempre
      implicit None
c         DECLARACAO DE VARIAVEIS
      real*8 Pi,Picalculado,diferenca,soma,Erro,E,Erromin
c     inteiro para receber o iostat,<0 eh final de arquivo, 0 significa dado correto, e >0 eh um erro
      integer i,stat
          Write(*,*)'Programa Calcula pi'
          write(*,*)'Data:30/06/2015'
          write(*,*)'Autor: Hildermes Jose Medeiros Filho'
          Write(*,*)'Picalculado e Pi=3,141592653589793'
          Write(*,*)'Erro' !Como Pi esta em funcao da soma e nao converge para Pi menor que 10**-8, pois a soma ja teria convergido a 10**-17
          Write(*,*)
c     LENDO O ERRO     
      E=-1.d0
      while(E.LT.0.d0)do
       write(*,*)'Digite E: '
        read(*,*,iostat=stat)E
        Erro=E
C     SOH ACEITO NUMEROS!      
       if(stat.GT.0)then
        Write(*,*)'Erro no dado de entrada'   
       END IF         
        write(*,*)
      END WHILE
C     Se o erro for muito pequeno      

C     CALCULOS      
      soma=0.d0
      pi=4.d0*DATAN(1.d0)
      picalculado=0.d0 
      i=0.d0
      diferenca=DABS(pi-picalculado)
      While(Diferenca.GT.Erro)do
        soma=soma+(-1.d0)**(i+2.d0)/(1.d0+2.d0*i)**(3.d0) !serie lenta se erro desejado for muito pequeno ira demorar
        i=i+1.d0
        picalculado=(soma*32.d0)**(1.d0/3.d0)
        Diferenca=ABS(pi-picalculado)
      end while 
c     RESULTADOS      
      write(*,*)'Foram necessarias, ',i,' iteracoes.'
      write(*,*)
      write(*,*)'Para um erro menor ou igual a ',Erro    
      Write(*,*)
      write(*,*)'Pi= ',pi
      Write(*,*)
      Write(*,*)'Picalculado= ',picalculado
      write(*,*)
      write(*,*)'A diferenca eh: ',diferenca
      write(*,*)
      Write(*,*)'Aperte uma tecla para fechar o programa.'
      read(*,*)
      
      END
       