      subroutine ELAND
     $(NMT,N,PFT,IPNT,RAT,NSUB)
C
C     Rudolf Loeser, 1984 Aug 16
C---- Gets selection indices, for DURHAM.
C     (This is version 2 of ELAND.)
C     !DASH
      save
C     !DASH
      real*8 PFT, PMAX, PMIN, RAT, RATIO
      integer IMAX, IMIN, IPNT, J, JMAX, JMIN, KODE, LUEO, N, NMT, NREP,
     $        NSUB
      logical MAXOK, MINOK
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external MINMAXD, DIVIDE, SINGD, MESHED, INDARRI, MASHED, HI, BYE
C
C               PFT(N,NMT), IPNT(NMT), RAT(NMT)
      dimension PFT(N,*),   IPNT(*),   RAT(*)
C     !EJECT
C
      call HI ('ELAND')
C     !BEG
      PMAX = -ZZLARGE
      JMAX = 0
      PMIN = +ZZLARGE
      JMIN = 0
      do 100 J = 1,NMT
        call MINMAXD (PFT(1,J), 1, N, IMIN, IMAX)
        call DIVIDE  (PFT(IMAX,J), PFT(IMIN,J), RATIO)
        RAT(J) = -RATIO
        if(PMAX.lt.PFT(IMAX,J)) then
          PMAX = PFT(IMAX,J)
          JMAX = J
        end if
        if(PMIN.gt.PFT(IMIN,J)) then
          PMIN = PFT(IMIN,J)
          JMIN = J
        end if
  100 continue
C
      call SINGD     (RAT, N, KODE, IPNT)
      if(KODE.ne.1) then
        call MESHED  ('ELAND', 3)
        write (LUEO,101)
  101   format(' ','Trouble selecting PF ratios for plotting.')
        call MASHED  ('ELAND')
C
        call INDARRI (IPNT, 1, 1, NSUB)
      end if
C
      MINOK = .false.
      MAXOK = .false.
      do 102 J = 1,NSUB
        if(IPNT(J).eq.JMAX) then
          MAXOK = .true.
        end if
        if(IPNT(J).eq.JMIN) then
          MINOK = .true.
        end if
  102 continue
C
      NREP = NSUB+1
      if(.not.MINOK) then
        NREP = NREP-1
        IPNT(NREP) = JMIN
      end if
      if(.not.MAXOK) then
        NREP = NREP-1
        IPNT(NREP) = JMAX
      end if
C     !END
      call BYE ('ELAND')
C
      return
      end
