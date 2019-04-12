      subroutine POLK
     $(NSL,IRS,KODE)
C
C     Rudolf Loeser, 1989 Aug 21
C---- Checks and adjusts switches IRS, for HOGRE.
C     !DASH
      save
C     !DASH
      integer I, IRS, NSL
      logical KODE
C     !DASH
      external HI, BYE
C
C               IRS(NSL)
      dimension IRS(*)
C
      call HI ('POLK')
C     !BEG
      KODE = .false.
C
      if(NSL.gt.0) then
C
        do 100 I = 1,NSL
          if(IRS(I).eq.0) then
            IRS(I) = 1
            KODE = .true.
          end if
  100   continue
C
      end if
C     !END
      call BYE ('POLK')
C
      return
      end
