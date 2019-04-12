      subroutine KOWRI
     $(XI,K,Y,A,W,IW,KODE)
C
C     Rudolf Loeser, 1968 May 29
C---- Controls the calculation of the summation weights A for the
C     approximation of an integral over half a line profile.
C
C---- Upon return, KODE = 1 if this calculation seems OK; = 0 if not.
C     !DASH
      save
C     !DASH
      real*8 A, ONE, W, XI, Y
      integer IFF, IGG, IN, IS, IW, K, KODE, MOX
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  LAURI, MYRTLE, WGIVE, HI, BYE
      intrinsic max,min
C
      dimension W(*), IW(*)
C
C               XI(K), A(K)
      dimension XI(*), A(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IFF   ),(IN( 2),IGG   )
C
      call HI ('KOWRI')
C     !BEG
      if(K.eq.1) then
        A(K) = ONE
        KODE = 1
      else
C       (Get, and allocate, W allotment)
        call LAURI  (IN, IS, MOX, 'KOWRI', K)
C
        call MYRTLE (XI, K, Y, A, W(IFF), W(IGG), W, IW, KODE)
C
C       (Give back W allotment)
        call WGIVE  (W, 'KOWRI')
      end if
      KODE = max(min(KODE,1),0)
C     !END
      call BYE ('KOWRI')
C
      return
      end
