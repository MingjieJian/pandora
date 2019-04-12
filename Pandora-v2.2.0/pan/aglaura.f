      subroutine AGLAURA
     $(XT,YT,NT,XF,YF,NF,QNAME,I1,I2)
C
C     Rudolf Loeser, 1977 Mar 25
C---- Checks on appropriateness of any extrapolations done in PUFF.
C     !DASH
      save
C     !DASH
      real*8 XF, XT, YF, YT
      integer I1, I2, LIB, LIE, NF, NT
      character QNAME*8
C     !DASH
      external DILLUS, DINSON, LAURA, HI, BYE
C
C               XT(NT), YT(NT), XF(NF), YF(NF)
      dimension XT(*),  YT(*),  XF(*),  YF(*)
C
      call HI ('AGLAURA')
C     !BEG
C---- Process start of table
      call DILLUS (XT, YT, NT, XF, YF, NF, LIB)
C---- Process end of table
      call DINSON (XT, YT, NT, XF, YF, NF, LIE)
C---- Print warnings as needed
      call LAURA  (YF(1), LIB, YF(NF), LIE, YT, NT, QNAME, I1, I2)
C     !END
      call BYE ('AGLAURA')
C
      return
      end
