      subroutine ZAP
     $(JAYTI,IU,IL,N,Z,K,DL,XJNU,NMAX,KMAX,NOLD,ZOLD,KOLD,DLOLD,XJOLD,
     $ XJOCP,XJINT,INTZ,FOUND)
C
C     Rudolf Loeser, 1980 Jun 06
C---- Reads and extra(inter)polates values of restart Jnu.
C     Input:   JAYTI, N, Z, K, DL; and also
C              IU, IL, for file positioning; and also
C              NMAX, KMAX, for consistency checking;
C     Output:  XJNU; and also
C              NOLD, ZOLD, KOLD, DLODL, XJOLD, INTZ, for printing.
C---- Note: If no values of XJNU(iu,il) are in the file, then the
C     elements of XJNU will retain their default values of zero.
C     (This is version 4 of ZAP.)
C     !DASH
      save
C     !DASH
      real*8 DL, DLOLD, XJINT, XJNU, XJOCP, XJOLD, Z, ZOLD
      integer IL, INTZ, IU, JAYTI, JL, JU, K, KMAX, KOLD, KOUNT, LUEO,
     $        N, NMAX, NOLD, NONC
      logical FOUND, GOOD, MATCH
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(29),NONC )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, ABORT, CABBAGE, CROTON, MOVE1, OKRA, THRASH,
     $         HI, BYE
C
C               ZOLD(NOLD), XJINT(N,KOLD), XJOLD(NOLD,KOLD), XJNU(N,K),
      dimension ZOLD(*),    XJINT(*),      XJOLD(*),         XJNU(*),
C
C               DLOLD(KOLD), XJOCP(NOLD,KOLD), Z(N), DL(K)
     $          DLOLD(*),    XJOCP(*),         Z(*), DL(*)
C     !EJECT
C
      call HI ('ZAP')
C     !BEG
      FOUND = .false.
C
      KOUNT = 0
  100 continue
      if(KOUNT.lt.NONC) then
C----   Read array sizes for the next data set in the file
        call THRASH   (JAYTI, NOLD, KOLD, GOOD)
        if(.not.GOOD) goto 102
        KOUNT = KOUNT+1
C
C----   Double-check array sizes
        if((NOLD.gt.NMAX).or.(KOLD.gt.KMAX)) then
          call MESHED ('ZAP', 1)
          write (LUEO,101) JAYTI,IU,IL,N,K,NOLD,KOLD,NMAX,KMAX
  101     format(' ','JAYTI',I4,'; looking for (',I2,'/',I2,').'//
     $           ' ',19X,'N',4X,'K'/
     $           ' ','for run        ',2I5/
     $           ' ','current set    ',2I5/
     $           ' ','max. from JAYTI',2I5)
          call ABORT
        end if
C
C----   Read current data set and its identifying transition
C       indices JU,JL
        call CABBAGE  (JAYTI, 2, NOLD, KOLD, JU, JL, ZOLD, DLOLD,
     $                 XJOLD)
        MATCH = (IU.eq.JU).and.(IL.eq.JL)
        if(.not.MATCH) then
C----     This is not the desired set, so read some more
          goto 100
        end if
C
C----   This is the desired set, so save it for printing, and
C       interpolate if necessary
        call MOVE1    (XJOLD, (NOLD*KOLD), XJOCP)
        call CROTON   (NOLD, N, INTZ)
        call OKRA     (N, Z, K, DL, XJNU, NOLD, ZOLD, KOLD, DLOLD,
     $                 XJOCP, XJINT, INTZ, IU, IL)
C
        FOUND = .true.
C
      end if
  102 continue
C     !END
      call BYE ('ZAP')
C
      return
      end
