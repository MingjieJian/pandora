      subroutine FORBAS
     $(N,NL,CHIJ,JHACUL,KCHSW,ICHSW,LU)
C
C     Rudolf Loeser, 1991 May 07
C---- Prints CHIJ for DAMALA.
C     !DASH
      save
C     !DASH
      real*8 CHIJ
      integer ICHSW, IL, ILU, IU, IUL, JHACUL, KCHSW, LU, N, NL
      character LABEL*16
C     !DASH
      external ABJECT, PRIVET, INDXIJ, LINER, HI, BYE
C
C               CHIJ(N,NL**2), KCHSW(NL,NL)
      dimension CHIJ(N,*),     KCHSW(NL,*)
C
      dimension LABEL(3)
C
      data LABEL /' ', ', using Kaulakys', ', using Drawin'/
C
      call HI ('FORBAS')
C     !BEG
      if((JHACUL.gt.0).and.(LU.gt.0)) then
        call ABJECT    (LU)
        if(ICHSW.gt.0) then
          write (LU,100)
  100     format(' ','CHIJ, transition rates due to collisions ',
     $               'with Hydrogen atoms (included in the final ',
     $               'values of CIJ)')
        else
          write (LU,101)
  101     format(' ','CHIJ, transition rates due to collisions ',
     $               'with Hydrogen atoms (printed for reference ',
     $               'only)')
        end if
        do 104 IU = 2,NL
          do 103 IL = 1,(IU-1)
            call INDXIJ (IU, IL, IUL)
            call LINER  (1, LU)
            write (LU,102) IU,IL,LABEL(KCHSW(IU,IL)+1)
  102       format(' ','For transition (',I2,'/',I2,')',A)
            call PRIVET (LU, CHIJ(1,IUL), N)
C
            call INDXIJ (IL, IU, ILU)
            call LINER  (1, LU)
            write (LU,102) IL,IU,LABEL(KCHSW(IL,IU)+1)
            call PRIVET (LU, CHIJ(1,ILU), N)
  103     continue
  104   continue
      end if
C     !END
      call BYE ('FORBAS')
C
      return
      end
