      subroutine PERCORD
     $(LUPD,VERSION,BEGDAT,BEGTIM,ENDDAT,ENDTIM,TYME0,QNAME,QELSM,
     $ IONST,QMODL,IOMX,ISUB,N,NL,NT,NONC,LUSD)
C
C     Rudolf Loeser, 1995 Oct 24
C---- Constructs an execution data record, and writes (i.e. appends)
C     it to the common run_archive file, on the DEC Alpha.
C
C---- Note: This action is quintessentially optional!! No one should
C     go to any trouble to transport this to another system, nor to
C     try to fix it if it breaks.
C
C     !DASH
      save
C     !DASH
      real*8 DAY, TIME, TYME0, VERSION, YEAR, dummy
      integer ID, IM, IOMX, IONST, IPRD, ISUB, ITM, IY, KN, LUPD, LUSD,
     $        N, NL, NONC, NT
      logical lummy
      character BEGDAT*11, BEGTIM*8, ELA*9, ENDDAT*11, ENDTIM*8, ION*8,
     $          LINE*128, MNTH*3, QELSM*8, QMODL*8, QNAME*8, SYST*19
C     !DASH
      external  MONTH, IONNAME, ELAPSED, SECOND, HOSTAGE
      intrinsic min, nint
C     !EJECT
C
C     !BEG
C---- Components of date
      read (ENDDAT,100) YEAR,MNTH,DAY
  100 format(F4.0,1X,A3,1X,F2.0)
      ID = DAY
      call MONTH   (1,MNTH,IM,lummy)
      IY = YEAR
C---- Compact form of ion name
      call IONNAME (QELSM,IONST,ION)
C---- PRD code
      IPRD = min(NONC,1)
C---- CPU time, in seconds
      call SECOND  (TIME)
      ITM = nint(TIME-TYME0)
C---- Elapsed time: hours, minutes, seconds
      call ELAPSED (BEGDAT,BEGTIM,ENDDAT,ENDTIM,dummy,ELA)
C---- Host name
      call HOSTAGE (0,LUSD,SYST)
C
C
C---- Construct data line
      write (LINE,101) ENDDAT,QMODL,ION(:4), IOMX,ISUB, IPRD,
     $                 N,NL,NT, ITM,ELA, VERSION,SYST
  101 format(A11,' ',A8,' ',A4,'; itr',I4,I3,'(',I1,'); n,nl,nt',I4,
     $       2I5,'; sec',I8,'; ela',A9,'; vrsn',F7.3,'; cpu ',A)
C
C
C---- Write line to standard output
      write (*,102) LINE
  102 format(A)
C
      if(LUPD.gt.0) then
C----   Attempt to write to data archive file
        KN = 0
  103   continue
          open (unit=LUPD, status='OLD', access='APPEND', err=105)
          write (LUPD,104) LINE
  104     format(A)
          close (unit=LUPD, disp='KEEP')
          goto 107
  105   continue
          KN = KN+1
          if(KN.gt.10) then
            write (*,106)
  106       format(' ','Cannot open Performance Data archive file.')
          else
            goto 103
          end if
  107   continue
      end if
C     !END
C
      return
      end
