      subroutine CROAT
     $(FRAT,PRAT,N,LABEL)
C
C     Rudolf Loeser, 1999 Jan 21
C---- Plots, for TROCAR.
C     !DASH
      save
C     !DASH
      real*8 FRAT, PRAT
      integer LUEO, N, NPNT, jummy
      character LABEL*(*), STAR*1
C     !COM
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(45),STAR  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external ISUA, REPAIR, DARUK, ABJECT, LINER, THADI, HI, BYE
C
C               FRAT(N), PRAT(N)
      dimension FRAT(*), PRAT(*)
C
      call HI ('CROAT')
C     !BEG
      call ISUA     (IMAGE,N)
      call REPAIR   (N,FRAT,PRAT,NPNT)
C
      if(NPNT.gt.(N/8)) then
        call DARUK  (IMAGE,FRAT,PRAT,N,jummy,STAR)
C
        call ABJECT (LUEO)
        write (LUEO,100) LABEL,LABEL
  100   format(' ','Plot of log(ratio-1) vs. depth index, where ',
     $             'ratio = ',A,'-new / ',A,'-old')
        call LINER  (1,LUEO)
        call THADI  (LUEO,IMAGE,N)
      end if
C     !END
      call BYE ('CROAT')
C
      return
      end
