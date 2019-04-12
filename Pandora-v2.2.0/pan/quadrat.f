      subroutine QUADRAT
     $(X,TAU,N,Y,TAURED,GDIL,TITLE,WN,W,IW,KODE)
C
C     Rudolf Loeser, 1989 Dec 06
C---- Computes WN by the "direct quadratic-representation" method.
C     Returns with KODE=1 if all seems OK,
C             with KODE=0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 TAU, W, WN, X, Y
      integer IMAX, IN, IS, ITNP, IW, IWK, K, KASE, KIL, KIS, KODE, MOX,
     $        N
      logical GDIL, TAURED
      character TITLE*(*)
C     !COM
C---- MUNUXI      as of 2005 Apr 14
      logical     LAMHED,LAMDMP
      common      /MUNUXI/ LAMHED,LAMDMP
C     Subroutine "LAMBDA" extra printout header control.
C     .
C     !DASH
      external RATTAN, LAUAN, IOTA, JILGA, ILETO, NATTER, WGIVE, SOTUR,
     $         NASAGI, JUNO, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               TAU(N), WN(N,N)
      dimension TAU(*), WN(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),ITNP  ),(IN( 2),IWK   )
C
      data KASE /1/
C     !EJECT
C
      call HI ('QUADRAT')
C     !BEG
C     (Get W allotment)
      call RATTAN    (IN, IS, MOX, 'QUADRAT', N)
C
      if(LAMDMP) then
        call JUNO
      end if
C---- Set up "reduced-TAU" table, TNP
      call NASAGI     (TAURED, TAU, N, W(ITNP), K, KIS, KIL, TITLE,
     $                 KODE, LAMDMP)
      call SOTUR      (1, KASE, KIS, KIL)
      if(KODE.gt.0) then
C----   Compute "reduced" WN matrix
        IMAX = K
        if(K.lt.N) then
          IMAX = K-2
        end if
        call LAUAN    (W(ITNP), IMAX, K, Y, W(IWK), TITLE, KODE, W, IW,
     $                 LAMDMP)
        if(KODE.gt.0) then
          if(LAMDMP) then
            call IOTA (W(IWK), K, TITLE, 'WN as computed')
          end if
C----     "Expand" WN matrix to final size
          call JILGA  (W(IWK), K, WN, N, TAU, KIS, KIL, TITLE, LAMDMP)
C----     Apply geometrical corrections (if needed)
          call ILETO  (X, GDIL, WN, N, TITLE, LAMDMP)
        end if
      end if
C---- Error advisory (if needed)
      call NATTER     (KODE, TITLE, Y, TAU, N, 'QUADRAT')
C
C     (Give back W allotment)
      call WGIVE      (W, 'QUADRAT')
C     !END
      call BYE ('QUADRAT')
C
      return
      end
