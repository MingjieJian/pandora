      subroutine VORPAL
     $(X,W,NW,WAVES,WVNUM,WTAB,LTYP,INDX,SCON,OPAC,IPRNT,IJECT,NO)
C
C     Rudolf Loeser, 1980 Jun 19
C---- Feeds Continuum "ECLIPSE" intensity and flux calculations.
C     (This is version 3 of VORPAL.)
C     !DASH
      save
C     !DASH
      real*8 OPAC, SCON, W, WAVES, WTAB, WVNUM, X
      integer IEOPAC, IESCON, IEWAVE, IEWTAB, IEWVNM, IJECT, IN, INDX,
     $        IPRNT, IS, LTYP, MOX, N, NEW, NO, NW, NZE
      logical OK
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(56),NZE)
C     !DASH
C     !EJECT
      external ANDAMAN, KLANG, HEARTH, RAJA, GRIFFIN, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               IPRNT(Nmkuse), WTAB(Nmkuse), SCON(N,Nmkuse), INDX(NZE),
      dimension IPRNT(*),      WTAB(*),      SCON(*),        INDX(*),
C
C               OPAC(N,Nmkuse), WAVES(Nmkuse), WVNUM(Nmkuse),
     $          OPAC(*),        WAVES(*),      WVNUM(*),
C
C               LTYP(Nmkuse)
     $          LTYP(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IEWAVE),(IN( 2),IESCON),(IN( 3),IEOPAC),(IN( 4),IEWVNM),
     $(IN( 5),IEWTAB)
C
      call HI ('VORPAL')
C     !BEG
      call ANDAMAN       (LTYP, NW, NEW)
      if(NEW.gt.0) then
C       (Get, and allocate, W allotment)
        call KLANG       (IN, IS, MOX, 'VORPAL', NEW)
C
C----   Pull out ECLIPSE data
        call HEARTH      (N, NW, NEW, LTYP, WAVES, WVNUM, WTAB, SCON,
     $                    OPAC, W(IEWAVE), W(IEWVNM), W(IEWTAB),
     $                    W(IESCON), W(IEOPAC), OK)
        if(OK) then
C----     Calculate
          if(NZE.gt.0) then
            call GRIFFIN (X, W, NEW, W(IEWAVE), W(IEWVNM), W(IEWTAB),
     $                    INDX, W(IESCON), W(IEOPAC), IJECT, NO)
          else
            call RAJA    (X, W, NEW, W(IEWAVE), W(IESCON), W(IEOPAC),
     $                    IPRNT, IJECT, NO)
          end if
        end if
C
C       (Give back W allotment)
        call WGIVE       (W, 'VORPAL')
      end if
C     !END
      call BYE ('VORPAL')
C
      return
      end
