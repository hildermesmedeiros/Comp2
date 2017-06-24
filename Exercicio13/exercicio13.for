      implicit none
      Real*8 x,y, menor, maior,quociente,quociente_int
      integer i, menor_int,primeiro_impar,impar_da_vez
      
      Write(*,*)'Programas: Numero de impares num intervalo'
      write(*,*)'Data:13/07/2015'
      write(*,*)'Autor: Hildermes Jose Medeiros Filho'      
      
c     leitura de x e Y

      Write(*,*)'Entre com os extremos do intervalo'
      read(*,*) x, y
      
c     identifiçao do menor e maior numero
      If (X.LT.Y) then
       menor=x
       maior=y
      else
       maior=y
       maior=x
      end if
      
      
c     verificar se o menor eh par ou impar

      quociente=menor/2.d0
      quociente_int=quociente
      If (quociente.EQ.DBLE(quociente_int)) then
       primeiro_impar=menor_int + 1
      else
      primeiro_impar=menor_int
      end If
      
c     verificar se menor_int eh par

      quociente=DBLE(menor_int)/2.d0
      quociente_int=quociente
      If (quociente.EQ.DBLE(quociente_int)) then
       primeiro_impar=menor_int + 1
      else
       primeiro_impar=menor_int + 2
      end If
      impar_da_vez = primeiro_impar
      i=0
      while (DBLE(impar_da_vez).LE.maior) do
       i=i + 1
       impar_da_vez=impar_da_vez + 2
      end while
        Write(*,*) ' O numero de impares no intervalo ',menor,' ate ',m
     &aior,' eh ',i
        
       write(*,*)
       write(*,*)'Aperte um tecla para fechar.'
       read(*,*)
      end                          
