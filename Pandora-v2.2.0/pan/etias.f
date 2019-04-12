      subroutine ETIAS
     $(KMAX,KMX,LAB,STOP)
C
C     Rudolf Loeser, 1991 Aug 26
C---- Checks counter limits, prints advice, and set a stop signal,
C     for HUDROs.
C     !DASH
      save
C     !DASH
      integer KMAX, KMX, LUEO
      logical STOP
      character LAB*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, MASHED, HI, BYE
C
      call HI ('ETIAS')
C     !BEG
      if(KMAX.gt.KMX) then
        call MESHED ('ETIAS', 1)
        write (LUEO,100) LAB,KMX,LAB,KMAX
  100   format(' ','The input value of ',A,'MAX =',I6,' is too small.'/
     $         ' ','The actual largest value of ',A,' =',I6)
C
        STOP = .true.
C
      else if(KMAX.lt.KMX) then
        call MESHED ('ETIAS', 3)
        write (LUEO,101) LAB,KMX,LAB,KMAX,LAB
  101   format(' ','The input value of ',A,'MAX =',I6,' is larger ',
     $             'than it needs to be.'/
     $         ' ','The actual largest value of ',A,' =',I6//
     $         ' ','Reducing the input value of ',A,'MAX ',
     $             'appropriately may reduce execution memory and ',
     $             'disk storage requirements.')
        call MASHED ('ETIAS')
      end if
C     !END
      call BYE ('ETIAS')
C
      return
      end
