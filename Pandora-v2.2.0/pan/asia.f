      subroutine ASIA
     $(XM,NL,XR,W,IW,D1L,DL1,TITLE,DMPI)
C
C     Rudolf Loeser, 1968 Jun 11
C---- Computes the determinant vectors, for AMERICA.
C     D(I,J) is the determinant of the (NL-1) x (NL-1) matrix formed by
C     eliminating row I and column J from the (NL) x (NL) matrix XM.
C     D1L is the vector D(1,L), and DL1 is the vector D(L,1), where
C     1 .le. L .le. NL, for both. Only these two component vectors of
C     D will actually be computed.
C     !DASH
      save
C     !DASH
      real*8 D1L, DL1, W, XM, XR
      integer IW, J, KODE, LUEO, NL, NLM
      logical DMPI
      character LABEL*80, TITLE*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MALAYA, ROTOR, ARABIA, MESHED, ABORT, HI, BYE
C
      dimension W(*), IW(*)
C
C               D1L(NL), DL1(NL), XM(NL,NL), XR(NL-1,NL-1)
      dimension D1L(*),  DL1(*),  XM(*),     XR(*)
C
      call HI ('ASIA')
C     !BEG
      NLM = NL-1
      write (LABEL,100) TITLE,1
  100 format(A,'  (',I3,')')
C
      call MALAYA     (XM, NL, 1, 1, XR)
      call ROTOR      (XR, NLM, LABEL, LUEO, W, IW, D1L(1), KODE)
      if(KODE.ne.1) then
        goto 102
      end if
      if(DMPI) then
        call ARABIA   (1, 1, XM, XR, NL, NLM, D1L(1))
      end if
C
      DL1(1) = D1L(1)
C     !EJECT
      do 101 J = 2,NL
        write (LABEL,100) TITLE,J
C
        call MALAYA   (XM, NL, 1, J, XR)
        call ROTOR    (XR, NLM, LABEL, LUEO, W, IW, D1L(J), KODE)
        if(KODE.ne.1) then
          goto 102
        end if
        if(DMPI) then
          call ARABIA (1, J, XM, XR, NL, NLM, D1L(J))
        end if
C
        call MALAYA   (XM, NL, J, 1, XR)
        call ROTOR    (XR, NLM, LABEL, LUEO, W, IW, DL1(J), KODE)
        if(KODE.ne.1) then
          goto 102
        end if
        if(DMPI) then
          call ARABIA (J, 1, XM, XR, NL, NLM, DL1(J))
        end if
  101 continue
      goto 104
C
  102 continue
      call MESHED     ('ASIA', 1)
      write (LUEO,103) KODE,LABEL
  103 format(' ','Determinant calculation failed. ',10X,'KODE =',I4/
     $       ' ',A//
     $       ' ','The NOVA method (METEP = 0) cannot be used for ',
     $           'LYMAN; choose another (METEP = 3 ?).')
      call ABORT
C
  104 continue
C     !END
      call BYE ('ASIA')
C
      return
      end
