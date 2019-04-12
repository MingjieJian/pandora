      subroutine TINAMOU
     $(Z,TE,VEC,YHZ,WS,SNU,DDL,INDL,LDL,LU)
C
C     Rudolf Loeser, 2004 Jun 17
C---- Supervises printout of line-core depths-of-formation.
C     (This is version 2 of TINAMOU.)
C     !DASH
      save
C     !DASH
      real*8 DDL, SNU, TE, VEC, WS, YHZ, Z
      integer I, INDL, J, LDL, LU, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external VECOUT, TOUCAN, HI, BYE
C
C               SNU(N,KM), WS(N,KM), INDL(LDLMX), DDL(LDLMX), YHZ(KM),
      dimension SNU(N,*),  WS(N,*),  INDL(*),     DDL(*),     YHZ(*),
C
C               VEC(N), TE(N), Z(N)
     $          VEC(*), TE(*), Z(*)
C
      call HI ('TINAMOU')
C     !BEG
      if(LU.gt.0) then
        call VECOUT   (LU, Z,  N, 'Z' )
        call VECOUT   (LU, TE, N, 'TE')
        do 100 I = 1,LDL
          J = INDL(I)
          call TOUCAN (LU, J, DDL(I), N, YHZ(J), WS(1,J), SNU(1,J), VEC)
  100   continue
      end if
C     !END
      call BYE ('TINAMOU')
C
      return
      end
