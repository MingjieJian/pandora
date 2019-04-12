      subroutine ERLIK
     $(QLAB,N,TAU,WNZ,CDSK,WHZ,WDSK,ILFLX)
C
C     Rudolf Loeser, 1981 Nov 03
C---- Prints, for FUROR.
C     !DASH
      save
C     !DASH
      real*8 CDSK, TAU, WDSK, WHZ, WNZ
      integer ILFLX, LUEO, N
      character QLAB*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, VECOUT, ARROUT, HI, BYE
C
C               TAU(N), WNZ(N,N), CDSK(N), WHZ(N,N), WDSK(N)
      dimension TAU(*), WNZ(*),   CDSK(*), WHZ(*),   WDSK(*)
C
      call HI ('ERLIK')
C     !BEG
      call LINER    (3, LUEO)
      write (LUEO,100) QLAB
  100 format(' ',5('----------'),5X,'for Ray: ',A8,5X,5('----------'))
C
      call VECOUT   (LUEO, TAU , N,    'TAU'     )
      call VECOUT   (LUEO, CDSK, N,    'C for WN')
      call ARROUT   (LUEO, WNZ , N, N, 'WN/Disk' )
C
      if(ILFLX.gt.0) then
        call VECOUT (LUEO, WDSK, N,    'C for WH')
        call ARROUT (LUEO, WHZ , N, N, 'WH/Disk' )
      end if
C     !END
      call BYE ('ERLIK')
C
      return
      end
