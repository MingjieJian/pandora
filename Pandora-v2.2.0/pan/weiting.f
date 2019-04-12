      subroutine WEITING
     $(ARP,ARU,ARW,N,WEIT,KODE,KMSS,TITLE)
C
C     Rudolf Loeser, 2002 Jan 24
C---- Weighting (with optional printout).
C
C     ARP(i) are the old values; ARU(i) are the new, unweighted values;
C     and ARW(i) will be the final, weighted values.
C
C     0 .le. WEIT(i) .le. 1;  WEIT applies to ARU (the new set).
C
C     KODE = 0: linear; KODE = 1: logarithmic where appropriate.
C
C     KMSS tells whether or not to print a record of this calculation.
C     !DASH
      save
C     !DASH
      real*8 ARP, ARU, ARW, WEIT
      integer I, K, KMSS, KODE, LUEO, N
      logical PRINT
      character STYLE*12, TITLE*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  WEIGHT, MESHED, VECOUT, MASHED, HI, BYE
      intrinsic min,max
C
C               ARP(N), ARU(N), ARW(N), WEIT(N)
      dimension ARP(*), ARU(*), ARW(*), WEIT(*)
C
      dimension STYLE(2)
C
      data STYLE /'linear.', 'logarithmic.'/
C     !EJECT
C
      call HI ('WEITING')
C     !BEG
      PRINT = KMSS.gt.0
      if(PRINT) then
        call MESHED ('WEITING', 3)
        K = max(min(KODE,1),0)+1
        write (LUEO,100) TITLE,STYLE(K)
  100   format(' ',A,' weighting, ',A,9X,'(Note input parameter IWEIT)')
        call VECOUT (LUEO, ARP , N, 'Old values'            )
        call VECOUT (LUEO, WEIT, N, 'Weights (of new)'      )
        call VECOUT (LUEO, ARU , N, 'Unweighted, new values')
      end if
C
C---- Compute the weighted values
      do 102 I = 1,N
        call WEIGHT (ARP(I), ARU(I), WEIT(I), KODE, ARW(I))
  102 continue
C
      if(PRINT) then
        call VECOUT (LUEO, ARW , N, 'Weighted, final values')
        call MASHED ('WEITING')
      end if
C     !END
      call BYE ('WEITING')
C
      return
      end
