      subroutine TOP
C
C     Rudolf Loeser, 1987 Oct 30
C---- Closes auxiliary input files.
C     !DASH
      save
C     !DASH
      integer LUAT, LUEO, LUGI, LUMO, LURE
C     !COM
C---- SUKU        as of 1988 Apr 22
      integer     NAF
      parameter   (NAF=5)
      integer     KFLOPN,KODEGN
      character   FILNMS*8, FILSPEC*60
      dimension   KFLOPN(NAF),FILNMS(NAF)
      common      /SUKU1/ KFLOPN
      common      /SUKU2/ FILNMS
      common      /SUKU3/ KODEGN,FILSPEC
C     Names and in-use codes for the main and the auxiliary
C     input files.
C     1=INPUT,  2=MODEL,  3=ATOM,  4=RESTART,  5=GENERAL.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 3),LUAT )
      equivalence (LUNITS( 2),LUMO )
      equivalence (LUNITS(20),LURE )
      equivalence (LUNITS(13),LUGI )
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LACK, HI, BYE
C
      call HI ('TOP')
C     !BEG
      if(KFLOPN(2).eq.1) then
        call LACK (LUMO, LUEO)
      end if
C
      if(KFLOPN(3).eq.1) then
        call LACK (LUAT, LUEO)
      end if
C
      if(KFLOPN(4).eq.1) then
        call LACK (LURE, LUEO)
      end if
C
      if(KFLOPN(5).eq.1) then
        call LACK (LUGI, LUEO)
      end if
C     !END
      call BYE ('TOP')
C
      return
      end
