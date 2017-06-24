c     Implicit None, primeiro comando, sempre
      implicit None
c         Declaracao de Variaveis
      real*8 A(100,100),soma_linha,soma_coluna
c     n, numero de elementos que tem a matrix A(n,n)
      Integer n
c     inteiro para receber o iostat,<0 eh final de arquivo, 0 significa dado correto, e >0 eh um erro
c     o problema eh que o valor do erro depende do tipo de sistema, cada fabricante usa o seu
c     exemplo: http://www.msg.ucsf.edu/local/programs/IBM_Compilers/Fortran/html/pgs/lr76.htm
      integer u,o
c     Linhas(L) e colunas(C)           
      integer L,C
c     Formats usados      
      character fmt*100,fmt_l*2,fmt_c*2 
          Write(*,*)'Programa que substitui a diagonal principal'
          write(*,*)'Data:30/06/2015'
          write(*,*)'Autor: Hildermes Jose Medeiros Filho'
          Write(*,*)'Se A(i,i) for nulo, vira a soma da coluna'
          Write(*,*)'caso contrario A(i,i) vira a soma da linha'
          Write(*,*)
c     Lendo n
      n=-1
      Write(*,*) 'A(n,n)'
      While(n.LE.0.OR.n.GT.100)Do
       Write(*,*)'Entre com N'      
       Read(*,*,iostat=u) n
c      se u>0 erro. n continua sendo -1 e volta a perguntar.       
       if(u.gt.0)then
        Write(*,*)'Erro, n precisa ser um inteiro 0<n<=100'
       end if     
      End While
          write(*,*)
c     os espaços na matriz me irritavam!Espero que tenha uma maneira mais facil
      write(*,*) 'Entre com A(i,j):'
      Do L=1,n
       if(L.LT.10)then
        write(fmt_L,*)'I1'
       elseif(L.LT.100)then
        write(fmt_L,*)'I2'
       else
        write(fmt_L,*)'I3'
       Endif
c      controle do iostat, se der erro vai voltar.
c     se receber o iostat de erro,ele vai para fora do loopin e imprime a msg de erro e a linha tem que ser redigitada  

c      Looping para a digitar a matriz termo a termo.
       Do c=1,n
  210  if(o.GT.0)then
        write(*,*)'A(i,j)=Numero real.Digite novamente.
     &'
       endif   
        if(c.LT.10)then
         write(fmt_c,*)'I1'
         fmt='(A2,'//fmt_l//',A1,'//fmt_c//',A1)'
         write(*,fmt)'A(',l,',',c,')'
         read(*,*,err=210,iostat=o) A(l,c)     
        else if(c.LT.100)then
         write(fmt_c,*)'I2'
         fmt='(A2,'//fmt_l//',A1,'//fmt_c//',A1)'
         write(*,fmt)'A(',l,',',c,')'
         read(*,*,err=210,IOSTAT=o) A(l,c)
        else
         write(fmt_c,*)'I3'
         fmt='(A2,'//fmt_l//',A1,'//fmt_c//',A1)'
         write(*,fmt)'A(',l,',',c,')'
         read(*,*,err=210,IOSTAT=o) A(l,c)
c       poderia resetar o loop com err, mas nao saberia o que esta acontecendo.          
        end if
       End Do         
      End Do 
      Write(*,300)'========Matriz_A======='
300   FORMAT(A50)
c     escrevendo a Matriz, fiz com 2 loopins interno.Precisei me preocupar com tamanho de n.Desta maneira, economizasse espaços na tela!
      write(fmt,*)'(',n,'F10.3',')'
      write(*,fmt)((A(l,c),c=1,n),l=1,n)  
c         
      Do l=1,n
        if(A(l,l).EQ.0.d0)Then
            Soma_coluna=0.d0
            Do c=1,n
                Soma_coluna=Soma_coluna+A(c,l)
            End Do        
                A(l,l)=Soma_coluna
        Else
            Soma_linha=0.d0
            Do c=1,n
                Soma_linha=Soma_linha+A(l,c)
            End Do
                A(l,l)=Soma_linha-A(l,l)        
        End if
      End Do
      write(*,*)          
      Write(*,300)'========Matriz_R======'
c escrevendo a matriz, fiz com looping da linha externo. Basta dar o formato de saida maximo, mais pratico!!! Porem, gasta mais espaços, pois sempre estrapola.      
      DO l=1,n
        write(*,30)(A(l,c),c=1,n)
30      Format(100F13.3)
      end do
      Write(*,*)'Aperte uma tecla para sair'        
      read(*,*)
      End          