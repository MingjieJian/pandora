      subroutine HOSTAGE
     $(LU,LUSD,SYST)
C
C     Rudolf Loeser, 2001 Nov 02
C---- Deals with host system information.
C     (See "Note" in subroutine PERCORD!)
C     !DASH
      save
C     !DASH
      integer K, LU, LUSD
      character LINE*80, NAV*13, SYST*(*)
C     !DASH
      intrinsic index
C
      data NAV /'not available'/
C
C     !BEG
      LINE = NAV
      SYST = NAV
C
      rewind LUSD
      read (LUSD,100,end=102) LINE
  100 format(A)
      if(LU.gt.0) then
C
        write (LU,101) LINE
  101   format(' ','cpu & system data: ',A)
C
      else
        K = index (LINE,' ')
        LINE = LINE(K+1:)
        K = index (LINE,' ')
        SYST = LINE(:K-1)
      end if
C
  102 continue
C     !END
C
      return
      end
