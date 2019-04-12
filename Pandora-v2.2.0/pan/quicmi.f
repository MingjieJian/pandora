      subroutine QUICMI
     $(KABS,KEMIT,KCSF,KOUT,LCON,LCSF,LSAV,LOUT)
C
C     Rudolf Loeser, 2004 Aug 19
C---- Sets switches for continuum calculations.
C     (This is version 4 of QUICMI.)
C     !DASH
      save
C     !DASH
      integer KABS, KCSF, KEMIT, KOUT
      logical LCON, LCSF, LOUT, LSAV
C     !DASH
      external HI, BYE
C
      call HI ('QUICMI')
C     !BEG
      LCON = ((KABS.gt.0).or.(KEMIT.gt.0))
      LCSF = (KCSF.gt.0)
      LSAV = (LCON.or.LCSF)
      LOUT = (KOUT.gt.0)
C     !END
      call BYE ('QUICMI')
C
      return
      end
