      subroutine BETH
     $(XLM,TAU,B,R,C,CX,Z,N,II,TAUR,BR,RR,CR,CXR,ZR,NR)
C
C     Rudolf Loeser, 1971 Feb 03
C---- Makes reduced tables, for PANKU.
C     !DASH
      save
C     !DASH
      real*8 B, BR, C, CR, CX, CXR, R, RR, TAU, TAUR, XLM, Z, ZERO, ZR
      integer II, K, LGT, LUEO, N, NR, NRM
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
      external  MOVE1, PLUSD, MESHED, VECOUT, ABORT, HI, BYE
      intrinsic max
C
C               TAU(N),  B(N),  R(N),  C(N),  Z(N),  CX(N),
      dimension TAU(*),  B(*),  R(*),  C(*),  Z(*),  CX(*),
C
C               TAUR(N), BR(N), RR(N), CR(N), ZR(N), CXR(N)
     $          TAUR(*), BR(*), RR(*), CR(*), ZR(*), CXR(*)
C     !EJECT
C
      call HI ('BETH')
C     !BEG
      NRM = NR-1
      if(II.gt.0) then
        K = max((II-1),1)
        TAUR(1) = ZERO
        call MOVE1  (TAU(II),NRM,TAUR(2))
        BR(1) = B(K)
        call MOVE1  (B(II)  ,NRM,BR(2)  )
        RR(1) = R(K)
        call MOVE1  (R(II)  ,NRM,RR(2)  )
        CR(1) = C(K)
        call MOVE1  (C(II)  ,NRM,CR(2)  )
        CXR(1) = CX(K)
        call MOVE1  (CX(II) ,NRM,CXR(2) )
        ZR(1) = Z(K)
        call MOVE1  (Z(II)  ,NRM,ZR(2)  )
      else
        call MOVE1  (TAU,N,TAUR)
        call MOVE1  (B  ,N,BR  )
        call MOVE1  (R  ,N,RR  )
        call MOVE1  (C  ,N,CR  )
        call MOVE1  (CX ,N,CXR )
        call MOVE1  (Z  ,N,ZR  )
      end if
C
      call PLUSD    (TAUR(2),1,NRM,LGT)
C
      if(LGT.lt.NRM) then
        call MESHED ('BETH',1)
        write (LUEO,100) XLM,II,NR,N
  100   format(' ','Trouble with reduced optical depth at Lambda =',
     $             1PE20.12/
     $         ' ','II =',I4,5X,'NR =',I4,5X,'N =',I4)
C
        call VECOUT (LUEO,TAU ,N ,'TAU')
        call VECOUT (LUEO,TAUR,NR,'TAUR')
        call ABORT
      end if
C     !END
      call BYE ('BETH')
C
      return
      end
