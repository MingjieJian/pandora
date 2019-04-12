      subroutine GNOME
     $(NO,HNDV,VNH,NVH,IVNH,ZGM,DGMZ,NGM,IDGMZ)
C
C     Rudolf Loeser, 1990 Aug 06
C---- Prints generating tables for atmosphere data.
C     !DASH
      save
C     !DASH
      real*8 DGMZ, HNDV, VNH, ZGM
      integer IDGMZ, IVNH, NGM, NO, NVH
      logical USED
C     !DASH
      external PADMA, VECOUT, LINER, HI, BYE
C
C               HNDV(NVH), VNH(NHV), ZGM(NGM), DGMZ(NGM)
      dimension HNDV(*),   VNH(*),   ZGM(*),   DGMZ(*)
C
      call HI ('GNOME')
C     !BEG
      USED = (IVNH.gt.0).or.(IDGMZ.gt.0)
      if(USED.and.(NO.gt.0)) then
        call PADMA    (NO, 'Master Tables')
C
        if(IVNH.gt.0) then
          write (NO,100)
  100     format(' ','Master Table of V(NH)')
          call VECOUT (NO, HNDV, NVH, 'HNDV')
          call VECOUT (NO, VNH,  NVH, 'VNH' )
        end if
C
        if(IDGMZ.gt.0) then
          call LINER  (2, NO)
          write (NO,101)
  101     format(' ','Master Table of DGM(Z)')
          call VECOUT (NO, ZGM,  NGM, 'ZGM' )
          call VECOUT (NO, DGMZ, NGM, 'DGMZ')
        end if
      end if
C     !END
      call BYE ('GNOME')
C
      return
      end
