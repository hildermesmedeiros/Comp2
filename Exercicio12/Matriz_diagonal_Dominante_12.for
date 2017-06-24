c     Implicit None, primeiro comando, sempre
      implicit None
c         Declaracao de Variaveis
      real*8 A(100,100),soma
c     n, numero de elementos que tem a matrix A(n,n)
      Integer n
c     inteiro para receber o iostat,<0 eh final de arquivo, 0 significa dado correto, e >0 eh um erro
c     o problema eh que o valor do erro depende do tipo de sistema, cada fabricante usa o seu
c     exemplo: http://www.msg.ucsf.edu/local/programs/IBM_Compilers/Fortran/html/pgs/lr76.htm
      integer u,o,verdadeiro,verlinha
c     Linhas(L) e colunas(C)           
      integer L,C
c     Formats usados      
      character fmt*100,fmt_l*2,fmt_c*2 
          Write(*,*)'Programa Matriz Diagonal Dominante'
          write(*,*)'Data:01/07/2015'
          write(*,*)'Autor: Hildermes Jose Medeiros Filho'
          Write(*,*)'Se o modulo de A(i,i) for maior que a soma dos mod
     &ulos de todos os demais elementos daquela linha, para todo linha 
     &da matriz'
          Write(*,*)' Matrizes com 2<=n<=100'
c     Lendo n
      n=-1
      Write(*,*) 'A(n,n)'
      While(n.LE.1.OR.n.GT.100)Do
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
      write(fmt,*)'(2x,',n,'F10.3',')'
      write(*,fmt)((A(l,c),c=1,n),l=1,n)
      write(*,*)
c     PROCESSANDO DADOS
      verdadeiro=0.d0
      Do l=1,n
       soma=0.d0       !soh me intereça saber a soma linha a linha
       verlinha=0.d0   !soh me intereça saber se eh verdadeiro linha a linha
       Do c=1,n
        if(DABS(A(l,l)).gt.DABS(A(l,c)))then
         Soma=soma+DABS(A(l,c))
         if(DABS(A(l,l)).gt.Soma)then
          verlinha=verlinha+1.d0   
         else
          verlinha=verlinha
         ENDIF
        endif
       end do
       if(verlinha.EQ.(n-1.d0))then  !como A(i,i) soh nao sera maior que ele mesmo, faço o teste.
        verdadeiro=verdadeiro+1.d0
       endif
      end do
      if(verdadeiro.eq.n)then !Como varri linha a linha, verdadeiro deve ser igual a numero de linhas.
       Write(*,*)'Esta Matriz eh de Diagonal estritamente dominante?'
       write(*,*)'SIM.'
      else
       Write(*,*)'Esta Matriz eh de diagonal estritamente Dominante?'
       write(*,*)'NAO.'
      END IF
       
      write(*,*)          

      Write(*,*)'Aperte uma tecla para sair'        
      read(*,*)
      End                
           