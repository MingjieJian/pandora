      subroutine MELANIA
     $(X,XLM,KOPAC)
C
C     Rudolf Loeser, 1995 Mar 02
C---- Sets the contribution switches for composite, statistical
C     and averaged Line Opacity; and resolves conflicts between them:
C     Only one of the three opacity options is used at XLM ---
C     averaged in preference to composite in preference to statistical.
C---- Note: when XLM = 0, then all switches are turned off.
C     (This is version 3 of MELANIA.)
C     !DASH
      save
C     !DASH
      real*8 ALM, X, XLM, ZERO
      integer KODE, KOPAC, L
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external  CAMEL, LAPSANA, SEYMOUR, HI, BYE
      intrinsic abs
C
      dimension X(*)
C
C               KOPAC(Nopac)
      dimension KOPAC(*)
C
      call HI ('MELANIA')
C     !BEG
      L = 1
C
      ALM = abs(XLM)
      if(ALM.gt.ZERO) then
C----   Averaged
        if((KOPAC(31).gt.0).or.(KOPAC(32).gt.0)) then
          call SEYMOUR     (ALM, KODE)
        else
          KODE = 0
        end if
        if(KODE.eq.1) then
          L = 4
        else
C----     Composite
          if((KOPAC(24).gt.0).or.(KOPAC(25).gt.0)) then
            call CAMEL     (X, ALM, KODE)
          else
            KODE = 0
          end if
          if(KODE.eq.1) then
            L = 2
          else
C----       Statistical
            if((KOPAC(22).gt.0).or.(KOPAC(23).gt.0)) then
              call LAPSANA (ALM, KODE)
            else
              KODE = 0
            end if
            if(KODE.eq.1) then
              L = 3
            end if
C
          end if
        end if
      end if
C     !EJECT
      goto (100, 101, 102, 103), L
  100 continue
C----   None
        KOPAC(22) = 0
        KOPAC(23) = 0
        KOPAC(24) = 0
        KOPAC(25) = 0
        KOPAC(31) = 0
        KOPAC(32) = 0
        goto 104
  101 continue
C----   Composite only
        KOPAC(22) = 0
        KOPAC(23) = 0
        KOPAC(31) = 0
        KOPAC(32) = 0
        goto 104
  102 continue
C----   Statistical only
        KOPAC(24) = 0
        KOPAC(25) = 0
        KOPAC(31) = 0
        KOPAC(32) = 0
        goto 104
  103 continue
C----   Averaged only
        KOPAC(22) = 0
        KOPAC(23) = 0
        KOPAC(24) = 0
        KOPAC(25) = 0
        goto 104
C
  104 continue
C     !END
      call BYE ('MELANIA')
C
      return
      end
