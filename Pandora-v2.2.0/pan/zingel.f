      subroutine ZINGEL
     $(K,N,MRR,SI,DI,SF,DF,TF,NVY,LABEL)
C
C     Rudolf Loeser, 1984 Dec 06
C---- Saves checksums of Intensity and Flux.
C     !DASH
      save
C     !DASH
      real*8 DF, DI, SF, SI, TF
      integer K, MRR, N, NVY
      character LABEL*1, TIT*24
C     !COM
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
C     !DASH
      external CHECKER, HI, BYE
C
C               SI(N,K), DI(MRR,K), SF(K), DF(K), TF(K)
      dimension SI(*),   DI(*),     SF(*), DF(*), TF(*)
C     !EJECT
C
      call HI ('ZINGEL')
C     !BEG
      write (TIT,100) LABEL,NVY,IU,IL
  100 format(A1,I3,' (',I2,'/',I2,') Profile: ')
C
      call CHECKER (SI,1,N*K  ,TIT//'Shell Intensity')
      call CHECKER (DI,1,MRR*K,TIT//'Disk Intensity')
      call CHECKER (SF,1,K    ,TIT//'Shell Flux')
      call CHECKER (DF,1,K    ,TIT//'Disk Flux')
      call CHECKER (TF,1,K    ,TIT//'Total Flux')
C     !END
      call BYE ('ZINGEL')
C
      return
      end
