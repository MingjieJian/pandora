      subroutine LAPSE
     $(QELSM,IONST,LABEL,N,EL,IST)
C
C     Rudolf Loeser, 2006 Mar 22
C---- Error message for CI calculations.
C     (This is version 2 of LAPSE.)
C     !DASH
      save
C     !DASH
      integer I, IONST, IST, LUEO, N
      character EL*2, LABEL*(*), QELSM*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, LINER, MASHED, HI, BYE
C
C               EL(N), IST(N)
      dimension EL(*), IST(*)
C
      call HI ('LAPSE')
C     !BEG
      call MESHED ('LAPSE', 3)
      write (LUEO,100) LABEL,QELSM,IONST
  100 format(' ','CI according to ',A,' cannot be computed for ',
     $           'ELSYM =',A,' and IONSTAGE =',I8//
     $       ' ','Data are available for the following:')
      call LINER  (1, LUEO)
      write (LUEO,101) (EL(I),IST(I),I=1,N)
  101 format(' ',10(A2,I3,5X)/)
      call MASHED ('LAPSE')
C     !END
      call BYE ('LAPSE')
C
      return
      end
