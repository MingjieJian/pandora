      subroutine MAPPING
     $(X,TAU,N,Y,TAURED,GDIL,TITLE,WN,W,IW,KODE)
C
C     Rudolf Loeser, 1989 Dec 06
C---- Computes WN by the "mapped quadratic-representation" method.
C     Returns with KODE=1 if all seems OK, with KODE=0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 TAU, W, WN, X, Y
      integer IB, IMM, IN, IS, ITNP, IVV, IW, IWK, IWS, IZZ, JJTS, JJXM,
     $        JN, K, KASE, KIL, KIS, KODE, M, MOX, MUX, N
      logical GDIL, TAURED
      character TITLE*(*)
C     !COM
C---- MUNUXI      as of 2005 Apr 14
      logical     LAMHED,LAMDMP
      common      /MUNUXI/ LAMHED,LAMDMP
C     Subroutine "LAMBDA" extra printout header control.
C     .
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 3),M  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 22),JJTS )
      equivalence (IZOQ( 95),JJXM )
C     !DASH
C     !EJECT
      external MADINGO, IMZAD, HOLLY, JILGA, ILETO, SOTUR, IOTA, WGIVE,
     $         MADANGI, NATTER, IGIVE, JUNO, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               TAU(N), WN(N,N)
      dimension TAU(*), WN(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),ITNP  ),(IN( 2),IZZ   ),(IN( 3),IVV   ),(IN( 4),IWK   )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IMM   )
C
      data KASE /2/
C
      call HI ('MAPPING')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call MADANGI  (IN, IS , MOX, 'MAPPING', N)
      call MADINGO  (JN, IWS, MUX, 'MAPPING', N)
C
      if(LAMDMP) then
        call JUNO
      end if
C---- Set up "reduced-TAU" table, TNP
      call IMZAD    (TAURED, TAU, N, W(ITNP), K, KIS, KIL, IB, TITLE,
     $               KODE, LAMDMP)
      call SOTUR    (1, KASE, KIS, KIL)
C
      if(KODE.gt.0) then
C----   Compute "reduced" WN matrix
        call HOLLY  (W(ITNP), K, X(JJTS), M, X(JJXM), W(IWK), IB,
     $               W(IZZ), IW(IMM), W(IVV))
        if(LAMDMP) then
          call IOTA ( W(IWK), K, TITLE, 'WN as computed')
        end if
C----   "Expand" WN matrix to final size
        call JILGA  (W(IWK), K, WN, N, TAU, KIS, KIL, TITLE, LAMDMP)
C----   Apply geometrical corrections (if needed)
        call ILETO  (X, GDIL, WN, N, TITLE, LAMDMP)
      end if
C
C---- Error advisory (if needed)
      call NATTER   (KODE, TITLE, Y, TAU, N, 'MAPPING')
C
C     (Give back W & IW allotments)
      call WGIVE    (W , 'MAPPING')
      call IGIVE    (IW, 'MAPPING')
C     !END
      call BYE ('MAPPING')
C
      return
      end
