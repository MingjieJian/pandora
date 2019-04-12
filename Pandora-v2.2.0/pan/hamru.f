      subroutine HAMRU
     $(Z,KZXST,TE,TCO,FCT,N,CTCO,CTMX,SHCOP,SHCOC,ZRCO,LU)
C
C     Rudolf Loeser, 2002 Feb 08
C---- Computes TCO, the carbon monoxide formation temperature.
C     !DASH
      save
C     !DASH
      real*8 ARG, CTCO, CTMX, DELZ, DIVC, DIVP, FAC, FCT, ONE, SHCOC,
     $       SHCOP, TCO, TE, TWO, Z, ZERO, ZRCO
      integer I, KZXST, LU, N
      logical CONE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
C     !EJECT
      external  ONE1, MOVE1, SHABBY, HI, BYE
      intrinsic min
C
C               Z(N), TE(N), TCO(N), FCT(N)
      dimension Z(*), TE(*), TCO(*), FCT(*)
C
      call HI ('HAMRU')
C     !BEG
      CONE = (CTCO.le.ZERO).or.(SHCOP.eq.ZERO).or.(SHCOC.eq.ZERO)
C
      if(CONE.or.(KZXST.eq.0)) then
        call ONE1   (FCT, N)
        call MOVE1  (TE , N, TCO)
      else
C
        DIVP = TWO*SHCOP
        DIVC = TWO*SHCOC
        do 100 I = 1,N
          DELZ = ZRCO-Z(I)
          if(Z(I).ge.ZRCO) then
            ARG = DELZ/DIVP
          else
            ARG = DELZ/DIVC
          end if
          FAC    = exp(ARG)
          FCT(I) = min(CTMX,(CTCO*FAC))
          TCO(I) = (ONE+FCT(I))*TE(I)
  100   continue
        call SHABBY (LU, SHCOP, SHCOC, ZRCO, CTMX, CTCO, N, TE, FCT,
     $               TCO)
C
      end if
C     !END
      call BYE ('HAMRU')
C
      return
      end
