c     Primeira linha de comando eh sempre
      Implicit NONE
c     DECLARACAO DE VARIAVEIS
      real*8 a,b,c,x1 ,x2, delta,partereal,parteimag
      integer ua, ub, uc, uaceita
      integer F
      character aceita*1,fmt*25
      
      Write(*,*)'Programa Equacao segundo grau'
          write(*,*)'Data:03/07/2015'
          write(*,*)'Autor: Hildermes Jose Medeiros Filho'
          Write(*,*)'Calcula raizes '
          Write(*,*)'a*x^2 + b*x + c'
          Write(*,*)'dados a, b e c'
          write(*,*)
      
      F=-1
      while(F.lt.0)DO
        write(*,*)'Digite a= '
        read(*,*,err=200,iostat=ua)a
        write(*,*)
        Write(*,*)'Digite b= '
        read(*,*,err=200,iostat=ub)b
        write(*,*)
        write(*,*)'Digite c= '
        read(*,*,err=200,iostat=uc)c
        write(*,*)
        write(fmt,*)'(F10.3,A7,F10.3,A5,F10.3)'
        write(*,fmt)a,'*x^2 + ',b,'*x + ',c
        write(*,*)
        write(*,*)'Esta equaca esta certa?'
        write(*,*)'Digite:S ou s,PARA SIM. Ou qualquer outra tecla para
     &redigitar a, b, c'
 100    FORMAT(A)
        read(*,100,err=200,iostat=uaceita)aceita
        write(aceita,*)aceita
        write(*,*)
        if(aceita.EQ.'S')then
         F=1 !se for sim, para
        elseif(aceita.EQ.'s')then 
         F=1 !se for sim, para
        else
        Write(*,*)'Digite a, b, c, novamente.'
        write(*,*)
         F=-1 !se nao for sim, repete
        END IF
 200    if(ua.gt.0)then
         Write(*,*)'Erro: a precisa ser um numero real.'
        elseif(ub.gt.0)then
         Write(*,*)'Erro: b precisa ser um numero real.' 
        elseif(uc.gt.0)then
         Write(*,*)'Erro: c precisa ser um numero real.'
        else if(uaceita.gt.0)then
         Write(*,*)'Digite S ou N.'
        end if
      END WHILE
       
      if(a.EQ.0.d0)then
       x1=(-c)/b
       write(*,*)'Esta eh uma equacao de 1 grau.'
      elseif(b.EQ.0.d0.AND.a.EQ.0.d0)then
       Write(*,*)'Isto eh uma constante.' 
      else
       delta=b**2.d0 -4.d0*a*c
       if(delta.LT.0.d0)then !duas raizes imaginarias
        partereal=(-b)/(2.d0*a)
        parteimag=DSQRT(DABS(delta))/(2.d0*a)
        write(*,*)'Foram achadas raizes imaginarias.'
        write(*,*)
        write(*,*)'x1= ',partereal,' +i ',parteimag
        write(*,*)'x2= ',partereal,' -i ',parteimag
       elseif(delta.GT.0.d0)then !duas raizes reais
        x1=(-b+DSQRT(delta))/(2.d0*a)
        x2=(-b-DSQRT(delta))/(2.d0*a)
        write(*,*)'x1= ',x1
        write(*,*)
        write(*,*)'x2= ',x2
       else !duas raizes iguais
        Write(*,*)'Duas raizes iguais'
        x1=(-b)/(2.d0*a)
        write(*,*)'x1=x2= ',x1
       ENDIF
      ENDIF 
       Write(*,*)
       write(*,*)'Aperte qualquer tecla para sair.'
       read(*,*)
      END  