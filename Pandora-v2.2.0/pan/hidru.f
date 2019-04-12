      subroutine HIDRU
     $(N,R1N,Z,KZXST,FRS,IQSFS,IQSFO)
C
C     Rudolf Loeser, 1982 Mar 31
C---- Computes FRS.
C     !DASH
      save
C     !DASH
      real*8 FRS, R, R1N, Z
      integer I, IQSFO, IQSFS, KZXST, N
C     !DASH
      external ONE1, HI, BYE
C
C               Z(N), FRS(N)
      dimension Z(*), FRS(*)
C
      call HI ('HIDRU')
C     !BEG
      if((IQSFS.gt.0).or.(IQSFO.gt.0)) then
        if(KZXST.gt.0) then
          R = R1N+Z(N)
          do 100 I = 1,N
            FRS(I) = (R-Z(I))/R1N
  100     continue
        else
          call ONE1 (FRS, N)
        end if
      end if
C     !END
      call BYE ('HIDRU')
C
      return
      end
