      subroutine PIETRO
     $(N,PREF,IMG,FO,LU,GOOD)
C
C     Rudolf Loeser, 1994 May 23
C---- Checks PREF,
C     and returns GOOD = .false. if values .le. 0 were edited out.
C
C     >>>>> Changes LU to LUEO if not GOOD, since the run will then
C     be aborted, and the extra output might be helpful.
C     !DASH
      save
C     !DASH
      real*8 FO, PREF, ZERO
      integer IMG, KERM, KMSS, LGT, LU, LUEO, N, NERM
      logical BAD, GOOD
      character LABEL*100
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external PLUSD, MESHED, DVECOUT, EDITH, HI, BYE
C
C               PREF(N), IMG(N), FO(N)
      dimension PREF(*), IMG(*), FO(*)
C
      data LABEL /'PREF, for Z-from-TAUKIN'/
      data KMSS,KERM,NERM /1, 0, 100/
C     !EJECT
C
      call HI ('PIETRO')
C     !BEG
      call PLUSD    (PREF, 1, N, LGT)
      GOOD = LGT.eq.N
C
      if(.not.GOOD) then
        call MESHED  ('PIETRO', 1)
        write (LUEO,100)
  100   format(' ','Computed new PREF is bad.'/
     $         ' ','It will be edited to allow the rest of this ',
     $             'calculation to proceed;'/
     $         ' ','but the run will be stopped after the printout ',
     $             'for this calculation is complete.')
        call DVECOUT (LUEO, PREF, N, 'PREF')
C
        call EDITH   (PREF, N, ZERO, 2, 1, KMSS, LABEL, IMG, FO,
     $                KERM, NERM, BAD)
        LU = LUEO
      end if
C     !END
      call BYE ('PIETRO')
C
      return
      end
