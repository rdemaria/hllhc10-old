c     25/10/2011 Stephane Fartoukh
c       From /afs/cern.ch/eng/lhc/optics/ATS_V6.503/toolkit/save_optics.f
c        and /afs/cern.ch/eng/lhc/optics/SLHC3.0/toolkit/save_optics.f
c     25/10/2011 R. De Maria
c       Adding triplet and matching section layout variables
c       adding flags for sextupole scheme and thin lens
c       adding new crossing scheme for crabs



      program save_optics

      implicit none
      integer ntot
      parameter(ntot=10000)
      real*8 ak(ntot),arc_squeeze,on_check,on_knob
      integer i

      open(1,file='temp/optics.input')
      open(2,file='temp/optics.madx')

      i=1
      do while(.true.)
      read(1,*,err=99,end=100) ak(i)
 99   i=i+1
      enddo
 100  continue
      write(6,*)

      i=1

      write(2,'(a15,1x,f3.1,a2)') 'ARC_SQUEEZE := ',ak( i),' ;'
      arc_squeeze=ak(i)
      i=i+1

      on_check=abs(ak(i))
      i=i+1

      on_knob=abs(ak(i))
      i=i+1

      write(2,*)
      write(2,*) '!***LAYOUT***'
      write(2,'(a15,1x,f20.10,a2)') 'l.MQXL      := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f20.10,a2)') 'l.MQX       := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f20.10,a2)') 'dq1q2a      := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f20.10,a2)') 'dq1aq1b     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f20.10,a2)') 'dq2aq2b     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f20.10,a2)') 'dq2bq3      := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f20.10,a2)') 'deltaposD2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f20.10,a2)') 'deltaposQ4  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f20.10,a2)') 'deltaposQ5  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f20.10,a2)') 'deltaposQ6  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,i9,a13)') 'on_cutMS.10 := ',int(ak( i)),' ;'
      i=i+1
      write(2,'(a15,1x,i9,a13)') 'on_cutMS.14 := ',int(ak( i)),' ;'
      i=i+1
      write(2,'(a15,1x,i9,a13)') 'is_thin     := ',int(ak( i)),' ;'
      i=i+1

      write(2,*)
      write(2,*) '!***BETAS in IR1 and IR5***'
      write(2,'(a15,1x,f16.6,a2)') 'betx_IP1     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f16.6,a2)') 'bety_IP1     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f16.6,a2)') 'betx_IP5     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f16.6,a2)') 'bety_IP5     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f16.6,a2)') 'betx0_IP1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f16.6,a2)') 'bety0_IP1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f16.6,a2)') 'betx0_IP5    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,f16.6,a2)') 'bety0_IP5    := ',ak( i),' ;'
      i=i+1

      write(2,*)
      write(2,*)'!***Exp. configuration in IR1, IR2, IR5 and IR8***'
      write(2,'(a30,f9.3,a30)')
     +'on_sep1:=0;on_x1:=0;phi_IR1 :=',ak( i),
     +'; on_sol_atlas:=0;on_crab1:=0;'
      i=i+1
      write(2,'(a30,f9.3,a30)')
     +'on_sep5:=0;on_x5:=0;phi_IR5 :=',ak( i),
     +'; on_sol_cms  :=0;on_crab5:=0;'
      i=i+1
      write(2,'(a48)')
     +'on_sep2:=0;on_x2:=0;on_alice:=0;on_sol_alice:=0;'
      write(2,'(a32)')
     +'on_sep8:=0;on_x8:=0;on_lhcb :=0;'
      write(2,*)
      write(2,'(a45)')
     +'abas:= 12.00/ 6.0*clight/(7E12)*on_sol_atlas;'
      write(2,'(a45)')
     +'abls:= 6.05/12.1*clight/(7E12)*on_sol_alice ;'
      write(2,'(a45)')
     +'abcs:= 52.00/13.0*clight/(7E12)*on_sol_cms  ;'

      write(2,*)
      write(2,*) '!***Ring Geometry***'
      write(2,*)
      write(2,*) '!Separation/recombination dipoles'
      write(2,'(a33)') 'kd1.lr1       :=  ad1.lr1/l.mbxa;'
      write(2,'(a33)') 'kd2.l1        :=  ad2.l5/l.mbrd ;'
      write(2,'(a33)') 'kd2.r1        :=  ad2.r5/l.mbrd ;'
      write(2,'(a33)') 'kd1.l2        :=  ad1.l2/l.mbx  ;'
      write(2,'(a33)') 'kd1.r2        :=  ad1.r2/l.mbx  ;'
      write(2,'(a33)') 'kd2.l2        :=  ad2.l2/l.mbrc ;'
      write(2,'(a33)') 'kd2.r2        :=  ad2.r2/l.mbrc ;'
      write(2,'(a33)') 'kd3.lr3       :=  ad3.lr3/l.mbw ;'
      write(2,'(a33)') 'kd4.lr3       :=  ad4.lr3/l.mbw ;'
      write(2,'(a33)') 'kd3.l4        :=  ad3.l4/l.mbrs ;'
      write(2,'(a33)') 'kd3.r4        :=  ad3.r4/l.mbrs ;'
      write(2,'(a33)') 'kd4.l4        :=  ad4.l4/l.mbrb ;'
      write(2,'(a33)') 'kd4.r4        :=  ad4.r4/l.mbrb ;'
      write(2,'(a33)') 'kd34.lr3      :=  ad3.lr3/l.mbw ;'
      write(2,'(a33)') 'kd34.lr7      :=  ad3.lr7/l.mbw ;'
      write(2,'(a33)') 'kd1.lr5       :=  ad1.lr5/l.mbxa;'
      write(2,'(a33)') 'kd2.l5        :=  ad2.l5/l.mbrd ;'
      write(2,'(a33)') 'kd2.r5        :=  ad2.r5/l.mbrd ;'
      write(2,'(a33)') 'kd3.lr7       :=  ad3.lr7/l.mbw ;'
      write(2,'(a33)') 'kd4.lr7       :=  ad4.lr7/l.mbw ;'
      write(2,'(a33)') 'kd1.l8        :=  ad1.l8/l.mbx  ;'
      write(2,'(a33)') 'kd1.r8        :=  ad1.r8/l.mbx  ;'
      write(2,'(a33)') 'kd2.l8        :=  ad2.l8/l.mbrc ;'
      write(2,'(a33)') 'kd2.r8        :=  ad2.r8/l.mbrc ;'
      write(2,'(a33)') 'ksumd2.l1b2   :=  kd2.l1        ;'
      write(2,'(a33)') 'ksumd2.l2b2   :=  kd2.l2        ;'
      write(2,'(a33)') 'ksumd2.l5b2   :=  kd2.l5        ;'
      write(2,'(a33)') 'ksumd2.l8b2   :=  kd2.l8        ;'
      write(2,'(a33)') 'ksumd2.r1b2   :=  kd2.l1        ;'
      write(2,'(a33)') 'ksumd2.r2b2   :=  kd2.l2        ;'
      write(2,'(a33)') 'ksumd2.r5b2   :=  kd2.l5        ;'
      write(2,'(a33)') 'ksumd2.r8b2   :=  kd2.l8        ;'
      write(2,*)
      write(2,*) '!Main dipoles'
      write(2,'(a33)') 'kb.a12        :=  ab.a12/l.mb   ;'
      write(2,'(a33)') 'kb.a23        :=  ab.a23/l.mb   ;'
      write(2,'(a33)') 'kb.a34        :=  ab.a34/l.mb   ;'
      write(2,'(a33)') 'kb.a45        :=  ab.a45/l.mb   ;'
      write(2,'(a33)') 'kb.a56        :=  ab.a56/l.mb   ;'
      write(2,'(a33)') 'kb.a67        :=  ab.a67/l.mb   ;'
      write(2,'(a33)') 'kb.a78        :=  ab.a78/l.mb   ;'
      write(2,'(a33)') 'kb.a81        :=  ab.a81/l.mb   ;'

      write(2,*)
      write(2,*) '!***IR1 Optics***'
      write(2,'(a15,1x,e20.12,a2)') 'KQX1.L1     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQX2a.L1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQX2b.L1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQX3.L1     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,a20,a2)')'KQX1.R1     := ','-KQX1.L1',' ;'
      write(2,'(a15,1x,a20,a2)')'KQX2a.R1    := ','-KQX2a.L1',' ;'
      write(2,'(a15,1x,a20,a2)')'KQX2b.R1    := ','-KQX2b.L1',' ;'
      write(2,'(a15,1x,a20,a2)')'KQX3.R1     := ','-KQX3.L1',' ;'
      write(2,*)
      write(2,*) '!Beam1'
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.L1B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.R1B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.L1B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.R1B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.L1B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.R1B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.L1B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.R1B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.L1B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.R1B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.L1B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.R1B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.L1B1   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.R1B1   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L1B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R1B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L1B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R1B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L1B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R1B1  := ',ak( i),' ;'
      i=i+1
      write(2,*)
      write(2,*) '!Beam2'
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.L1B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.R1B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.L1B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.R1B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.L1B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.R1B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.L1B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.R1B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.L1B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.R1B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.L1B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.R1B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.L1B2   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.R1B2   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L1B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R1B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L1B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R1B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L1B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R1B2  := ',ak( i),' ;'
      i=i+1
      write(2,*)
      write(2,*) '!***IR1 X-scheme***'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV1.L1x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV1.L1s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXV1.L1     := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV1.R1x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV1.R1s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXV1.R1     := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH1.L1x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH1.L1s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXH1.L1     := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH1.R1x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH1.R1s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXH1.R1     := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
       write(2,'(a17,1x,e20.12,a2)') 'ACBXV2.L1x    := ',ak( i),';'
       i=i+1
       write(2,'(a17,1x,e20.12,a2)') 'ACBXV2.L1s    := ',ak( i),';'
       i=i+1
       write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXV2.L1     := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV2.R1x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV2.R1s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXV2.R1     := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH2.L1x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH2.L1s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXH2.L1     := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH2.R1x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH2.R1s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXH2.R1     := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV3.L1x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV3.L1s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXV3.L1     := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV3.R1x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV3.R1s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXV3.R1     := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH3.L1x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH3.L1s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXH3.L1     := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH3.R1x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH3.R1s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXH3.R1     := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,*)
      write(2,*) '!Beam1'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.L1B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.L1B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDV4.L1B1  := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.R1B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.R1B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDV4.R1B1  := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.L1B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.L1B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYVS4.L1B1  := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.R1B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.R1B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYVS4.R1B1  := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV5.L1B1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV5.L1B1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCV5.L1B1   := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV6.R1B1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV6.R1B1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCV6.R1B1   := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.L1B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.L1B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDH4.L1B1  := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.R1B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.R1B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDH4.R1B1  := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.L1B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.L1B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYHS4.L1B1  := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.R1B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.R1B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYHS4.R1B1  := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH6.L1B1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH6.L1B1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCH6.L1B1   := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH5.R1B1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH5.R1B1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCH5.R1B1   := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,*)
      write(2,*) '!Beam2'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.L1B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.L1B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDV4.L1B2  := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.R1B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.R1B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDV4.R1B2  := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.L1B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.L1B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYVS4.L1B2  := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.R1B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.R1B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYVS4.R1B2  := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV6.L1B2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV6.L1B2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCV6.L1B2   := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV5.R1B2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV5.R1B2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCV5.R1B2   := (',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.L1B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.L1B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDH4.L1B2  := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.R1B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.R1B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDH4.R1B2  := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.L1B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.L1B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYHS4.L1B2  := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.R1B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.R1B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYHS4.R1B2  := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH5.L1B2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH5.L1B2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCH5.L1B2   := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH6.R1B2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH6.R1B2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCH6.R1B2   := (',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1 +(',
     +                     ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1 ;'
      write(2,*)
      write(2,*) '!***CRAB CAVITIES in IR1***'
      write(2,'(a15,1x,e20.12,a2)') 'AHCRAB_L1B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'AHCRAB_R1B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'AHCRAB_L1B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'AHCRAB_R1B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'AVCRAB_L1B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'AVCRAB_R1B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'AVCRAB_L1B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'AVCRAB_R1B2 := ',ak( i),' ;'
      i=i+1
      write(2,*)
      write(2,*) '!***IR5 Optics***'
      write(2,'(a15,1x,e20.12,a2)') 'KQX1.L5     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQX2a.L5    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQX2b.L5    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQX3.L5     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,a20,a2)')'KQX1.R5     := ','-KQX1.L5',' ;'
      write(2,'(a15,1x,a20,a2)')'KQX2a.R5    := ','-KQX2a.L5',' ;'
      write(2,'(a15,1x,a20,a2)')'KQX2b.R5    := ','-KQX2b.L5',' ;'
      write(2,'(a15,1x,a20,a2)')'KQX3.R5     := ','-KQX3.L5',' ;'
      write(2,*)
      write(2,*) '!Beam1'
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.L5B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.R5B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.L5B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.R5B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.L5B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.R5B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.L5B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.R5B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.L5B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.R5B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.L5B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.R5B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.L5B1   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.R5B1   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L5B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R5B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L5B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R5B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L5B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R5B1  := ',ak( i),' ;'
      i=i+1
      write(2,*)
      write(2,*) '!Beam2'
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.L5B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.R5B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.L5B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.R5B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.L5B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.R5B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.L5B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.R5B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.L5B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.R5B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.L5B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.R5B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.L5B2   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.R5B2   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L5B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R5B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L5B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R5B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L5B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R5B2  := ',ak( i),' ;'
      i=i+1
      write(2,*)
      write(2,*) '!***IR5 X-scheme***'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV1.L5x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV1.L5s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXV1.L5     := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV1.R5x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV1.R5s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXV1.R5     := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH1.L5x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH1.L5s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXH1.L5     := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH1.R5x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH1.R5s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXH1.R5     := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV2.L5x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV2.L5s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXV2.L5     := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV2.R5x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV2.R5s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXV2.R5     := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH2.L5x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH2.L5s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXH2.L5     := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH2.R5x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH2.R5s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXH2.R5     := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV3.L5x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV3.L5s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXV3.L5     := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV3.R5x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXV3.R5s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXV3.R5     := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH3.L5x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH3.L5s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXH3.L5     := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH3.R5x    := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBXH3.R5s    := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBXH3.R5     := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,*)
      write(2,*) '!Beam1'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.L5B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.L5B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDV4.L5B1  := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.R5B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.R5B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDV4.R5B1  := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.L5B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.L5B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYVS4.L5B1  := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.R5B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.R5B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYVS4.R5B1  := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV5.L5B1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV5.L5B1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCV5.L5B1   := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV6.R5B1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV6.R5B1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCV6.R5B1   := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.L5B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.L5B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDH4.L5B1  := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.R5B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.R5B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDH4.R5B1  := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.L5B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.L5B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYHS4.L5B1  := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.R5B1x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.R5B1s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYHS4.R5B1  := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH6.L5B1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH6.L5B1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCH6.L5B1   := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH5.R5B1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH5.R5B1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCH5.R5B1   := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,*)
      write(2,*) '!Beam2'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.L5B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.L5B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDV4.L5B2  := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.R5B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDV4.R5B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDV4.R5B2  := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.L5B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.L5B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYVS4.L5B2  := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.R5B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYVS4.R5B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYVS4.R5B2  := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV6.L5B2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV6.L5B2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCV6.L5B2   := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV5.R5B2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCV5.R5B2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCV5.R5B2   := (',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.L5B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.L5B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDH4.L5B2  := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.R5B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBRDH4.R5B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBRDH4.R5B2  := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.L5B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.L5B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYHS4.L5B2  := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.R5B2x := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBYHS4.R5B2s := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBYHS4.R5B2  := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH5.L5B2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH5.L5B2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCH5.L5B2   := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH6.R5B2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'ACBCH6.R5B2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a35)')
     +'ACBCH6.R5B2   := (',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5 +(',
     +                     ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5 ;'
      write(2,*)
      write(2,*) '!***CRAB CAVITIES in IR5***'
      write(2,'(a15,1x,e20.12,a2)') 'AHCRAB_L5B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'AHCRAB_R5B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'AHCRAB_L5B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'AHCRAB_R5B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'AVCRAB_L5B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'AVCRAB_R5B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'AVCRAB_L5B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'AVCRAB_R5B2 := ',ak( i),' ;'
      i=i+1
      write(2,*)

      write(2,*) '!***IR2 Optics***'
      write(2,'(a15,1x,e20.12,a2)') 'KQX.L2      := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KTQX1.L2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KTQX2.L2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,a20,a2)')'KQX.R2      := ','-KQX.L2  ',' ;'
      write(2,'(a15,1x,a20,a2)')'KTQX1.R2    := ','-KTQX1.L2',' ;'
      write(2,'(a15,1x,a20,a2)')'KTQX2.R2    := ','-KTQX2.L2',' ;'
      write(2,*)
      write(2,*) '!Beam1'
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.L2B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.R2B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.L2B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.R2B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.L2B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.R2B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.L2B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.R2B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.L2B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.R2B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.L2B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.R2B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.L2B1   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.R2B1   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L2B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R2B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L2B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R2B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L2B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R2B1  := ',ak( i),' ;'
      i=i+1
      write(2,*)
      write(2,*) '!Beam2'
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.L2B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.R2B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.L2B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.R2B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.L2B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.R2B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.L2B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.R2B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.L2B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.R2B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.L2B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.R2B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.L2B2   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.R2B2   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L2B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R2B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L2B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R2B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L2B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R2B2  := ',ak( i),' ;'
      i=i+1
      write(2,*)
      write(2,*) '!***IR2 X-scheme***'
      write(2,'(a53)')
     +'abxwt.l2      := -0.0000772587268993839836*on_alice ;'
      write(2,'(a53)')
     +'abwmd.l2      := +0.0001472587268993839840*on_alice ;'
      write(2,'(a53)')
     +'abaw.r2       := -0.0001335474860334838000*on_alice ;'
      write(2,'(a53)')
     +'abxwt.r2      := +0.0000635474860334838004*on_alice ;'
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXV1.L2     := ',ak( i),'*on_x2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXV1.R2     := ',ak( i),'*on_x2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXH1.L2     := ',ak( i),'*on_sep2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXH1.R2     := ',ak( i),'*on_sep2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXV2.L2     := ',ak( i),'*on_x2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXV2.R2     := ',ak( i),'*on_x2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXH2.L2     := ',ak( i),'*on_sep2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXH2.R2     := ',ak( i),'*on_sep2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXV3.L2     := ',ak( i),'*on_x2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXV3.R2     := ',ak( i),'*on_x2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXH3.L2     := ',ak( i),'*on_sep2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXH3.R2     := ',ak( i),'*on_sep2;'
      i=i+1
      write(2,*)
      write(2,*) '!Beam1'
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYVS4.L2B1  := ',ak( i),'*on_x2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYVS4.R2B1  := ',ak( i),'*on_x2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYVS5.L2B1  := ',ak( i),'*on_x2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBCVS5.R2B1  := ',ak( i),'*on_x2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYHS4.L2B1  := ',ak( i),'*on_sep2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYHS4.R2B1  := ',ak( i),'*on_sep2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYHS5.L2B1  := ',ak( i),'*on_sep2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBCHS5.R2B1  := ',ak( i),'*on_sep2;'
      i=i+1
      write(2,*)
      write(2,*) '!Beam2'
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYVS4.L2B2  := ',ak( i),'*on_x2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYVS4.R2B2  := ',ak( i),'*on_x2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYVS5.L2B2  := ',ak( i),'*on_x2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBCVS5.R2B2  := ',ak( i),'*on_x2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYHS4.L2B2  := ',ak( i),'*on_sep2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYHS4.R2B2  := ',ak( i),'*on_sep2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYHS5.L2B2  := ',ak( i),'*on_sep2;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBCHS5.R2B2  := ',ak( i),'*on_sep2;'
      i=i+1

      write(2,*)
      write(2,*) '!***IR8 Optics***'
      write(2,'(a15,1x,e20.12,a2)') 'KQX.L8      := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KTQX1.L8    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KTQX2.L8    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,a20,a2)')'KQX.R8      := ','-KQX.L8  ',' ;'
      write(2,'(a15,1x,a20,a2)')'KTQX1.R8    := ','-KTQX1.L8',' ;'
      write(2,'(a15,1x,a20,a2)')'KTQX2.R8    := ','-KTQX2.L8',' ;'
      write(2,*)
      write(2,*) '!Beam1'
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.L8B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.R8B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.L8B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.R8B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.L8B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.R8B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.L8B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.R8B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.L8B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.R8B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.L8B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.R8B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.L8B1   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.R8B1   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L8B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R8B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L8B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R8B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L8B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R8B1  := ',ak( i),' ;'
      i=i+1
      write(2,*)
      write(2,*) '!Beam2'
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.L8B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.R8B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.L8B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.R8B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.L8B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.R8B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.L8B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.R8B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.L8B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.R8B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.L8B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.R8B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.L8B2   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.R8B2   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L8B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R8B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L8B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R8B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L8B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R8B2  := ',ak( i),' ;'
      i=i+1
      write(2,*)
      write(2,*) '!***IR8 X-scheme***'
      write(2,'(a53)')
     +'abxws.l8      := -0.000045681598453109894*on_lhcb   ;'
      write(2,'(a53)')
     +'abxwh.l8      := +0.000180681598453109894*on_lhcb   ;'
      write(2,'(a53)')
     +'ablw.r8       := -0.000180681598453109894*on_lhcb   ;'
      write(2,'(a53)')
     +'abxws.r8      := +0.000045681598453109894*on_lhcb   ;'
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXV1.L8     := ',ak( i),'*on_sep8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXV1.R8     := ',ak( i),'*on_sep8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXH1.L8     := ',ak( i),'*on_x8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXH1.R8     := ',ak( i),'*on_x8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXV2.L8     := ',ak( i),'*on_sep8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXV2.R8     := ',ak( i),'*on_sep8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXH2.L8     := ',ak( i),'*on_x8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXH2.R8     := ',ak( i),'*on_x8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXV3.L8     := ',ak( i),'*on_sep8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXV3.R8     := ',ak( i),'*on_sep8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXH3.L8     := ',ak( i),'*on_x8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBXH3.R8     := ',ak( i),'*on_x8;'
      i=i+1
      write(2,*)
      write(2,*) '!Beam1'
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYVS4.L8B1  := ',ak( i),'*on_sep8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYVS4.R8B1  := ',ak( i),'*on_sep8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBCVS5.L8B1  := ',ak( i),'*on_sep8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYVS5.R8B1  := ',ak( i),'*on_sep8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYHS4.L8B1  := ',ak( i),'*on_x8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYHS4.R8B1  := ',ak( i),'*on_x8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBWH5.L8B1   := ',ak( i),'*on_x8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBCH5.L8B1   := ',ak( i),'*on_x8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBCHS5.L8B1  := ',ak( i),'*on_x8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYHS5.R8B1  := ',ak( i),'*on_x8;'
      i=i+1
      write(2,*)
      write(2,*) '!Beam2'
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYVS4.L8B2  := ',ak( i),'*on_sep8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYVS4.R8B2  := ',ak( i),'*on_sep8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBCVS5.L8B2  := ',ak( i),'*on_sep8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYVS5.R8B2  := ',ak( i),'*on_sep8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYHS4.L8B2  := ',ak( i),'*on_x8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYHS4.R8B2  := ',ak( i),'*on_x8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBCHS5.L8B2  := ',ak( i),'*on_x8;'
      i=i+1
      write(2,'(a17,1x,e20.12,a10)')
     +'ACBYHS5.R8B2  := ',ak( i),'*on_x8;'
      i=i+1

      write(2,*)
      write(2,*) '!***IR4 Optics***'
      write(2,*)
      write(2,*) '!Beam1'
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.L4B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.R4B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.L4B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.R4B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.L4B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.R4B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.L4B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.R4B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.L4B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.R4B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.L4B1   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.R4B1   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L4B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R4B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L4B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R4B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L4B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R4B1  := ',ak( i),' ;'
      i=i+1
      write(2,*)
      write(2,*) '!Beam2'
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.L4B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.R4B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.L4B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.R4B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.L4B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ7.R4B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.L4B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.R4B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.L4B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.R4B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.L4B2   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.R4B2   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L4B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R4B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L4B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R4B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L4B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R4B2  := ',ak( i),' ;'
      i=i+1

      write(2,*)
      write(2,*) '!***IR6 Optics***'
      write(2,*)
      write(2,*) '!Beam1'
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.L6B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.R6B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.L6B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.R6B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.L6B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.R6B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.L6B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.R6B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.L6B1   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.R6B1   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L6B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R6B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L6B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R6B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L6B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R6B1  := ',ak( i),' ;'
      write(2,*)
      write(2,*) '!Beam2'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.L6B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.R6B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.L6B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.R6B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.L6B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ8.R6B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.L6B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ9.R6B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.L6B2   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ10.R6B2   := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L6B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R6B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L6B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R6B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L6B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R6B2  := ',ak( i),' ;'
      i=i+1

      write(2,*)
      write(2,*) '!***IR3 Optics***'
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.LR3     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT4.L3     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT4.R3     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.LR3     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT5.L3     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT5.R3     := ',ak( i),' ;'
      i=i+1
      write(2,*)
      write(2,*) '!Beam1'
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.L3B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.R3B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL7.L3B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL7.R3B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL8.L3B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL8.R3B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL9.L3B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL9.R3B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL10.L3B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL10.R3B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L3B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R3B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L3B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R3B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L3B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R3B1  := ',ak( i),' ;'
      i=i+1
      write(2,*)
      write(2,*) '!Beam2'
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.L3B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.R3B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL7.L3B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL7.R3B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL8.L3B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL8.R3B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL9.L3B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL9.R3B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL10.L3B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL10.R3B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L3B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R3B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L3B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R3B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L3B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R3B2  := ',ak( i),' ;'
      i=i+1

      write(2,*)
      write(2,*) '!***IR7 Optics***'
      write(2,'(a15,1x,e20.12,a2)') 'KQ4.LR7     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT4.L7     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT4.R7     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ5.LR7     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT5.L7     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT5.R7     := ',ak( i),' ;'
      i=i+1
      write(2,*)
      write(2,*) '!Beam1'
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.L7B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.R7B1    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL7.L7B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL7.R7B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL8.L7B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL8.R7B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL9.L7B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL9.R7B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL10.L7B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL10.R7B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L7B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R7B1 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L7B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R7B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L7B1  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R7B1  := ',ak( i),' ;'
      i=i+1
      write(2,*)
      write(2,*) '!Beam2'
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.L7B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQ6.R7B2    := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL7.L7B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL7.R7B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL8.L7B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL8.R7B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL9.L7B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL9.R7B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL10.L7B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL10.R7B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.L7B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQTL11.R7B2 := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.L7B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT12.R7B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.L7B2  := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQT13.R7B2  := ',ak( i),' ;'
      i=i+1

      write(2,*)
      write(2,*) '!***Arc Optics***'
      write(2,*) '!QF/QD'
      write(2,'(a15,1x,e20.12,a2)') 'KQF.A81     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQF.A12     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQF.A45     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQF.A56     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQD.A81     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQD.A12     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQD.A45     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQD.A56     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQF.A78     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQF.A23     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQF.A34     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQF.A67     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQD.A78     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQD.A23     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQD.A34     := ',ak( i),' ;'
      i=i+1
      write(2,'(a15,1x,e20.12,a2)') 'KQD.A67     := ',ak( i),' ;'
      i=i+1

      if (on_knob.eq.0) then
      write(2,*)
      write(2,*) '!QTF/QTD BEAM1'
      write(2,'(a15,1x,e20.12,a2)') 'KQTF.B1     := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'KQTD.B1     := ',0.,' ;'
      if (arc_squeeze<0.1) then
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A81B1  := ',ak( i),' + KQTF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A12B1  := ',ak( i),' + KQTF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A45B1  := ',ak( i),' + KQTF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A56B1  := ',ak( i),' + KQTF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A81B1  := ',ak( i),' + KQTD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A12B1  := ',ak( i),' + KQTD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A45B1  := ',ak( i),' + KQTD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A56B1  := ',ak( i),' + KQTD.B1;'
      i=i+5
      else
      write(2,'(a15,1x,e20.12,a2)')'KQTF.A81B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KQTF.A12B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KQTF.A45B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KQTF.A56B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KQTD.A81B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KQTD.A12B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KQTD.A45B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KQTD.A56B1  := ',ak( i),' ;'
      i=i+5
      endif
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A78B1  := ',ak( i),' + KQTF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A23B1  := ',ak( i),' + KQTF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A34B1  := ',ak( i),' + KQTF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A67B1  := ',ak( i),' + KQTF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A78B1  := ',ak( i),' + KQTD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A23B1  := ',ak( i),' + KQTD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A34B1  := ',ak( i),' + KQTD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A67B1  := ',ak( i),' + KQTD.B1;'
      i=i+5

      write(2,*)
      write(2,*) '!QTF/QTD BEAM2'
      write(2,'(a15,1x,e20.12,a2)') 'KQTF.B2     := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'KQTD.B2     := ',0.,' ;'
      if (arc_squeeze<0.1) then
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A81B2  := ',ak( i),' + KQTF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A12B2  := ',ak( i),' + KQTF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A45B2  := ',ak( i),' + KQTF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A56B2  := ',ak( i),' + KQTF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A81B2  := ',ak( i),' + KQTD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A12B2  := ',ak( i),' + KQTD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A45B2  := ',ak( i),' + KQTD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A56B2  := ',ak( i),' + KQTD.B2;'
      i=i+5
      else
      write(2,'(a15,1x,e20.12,a2)')'KQTF.A81B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KQTF.A12B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KQTF.A45B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KQTF.A56B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KQTD.A81B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KQTD.A12B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KQTD.A45B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KQTD.A56B2  := ',ak( i),' ;'
      i=i+5
      endif
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A78B2  := ',ak( i),' + KQTF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A23B2  := ',ak( i),' + KQTF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A34B2  := ',ak( i),' + KQTF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTF.A67B2  := ',ak( i),' + KQTF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A78B2  := ',ak( i),' + KQTD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A23B2  := ',ak( i),' + KQTD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A34B2  := ',ak( i),' + KQTD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')
     +'KQTD.A67B2  := ',ak( i),' + KQTD.B2;'
      i=i+5

      write(2,*)
      write(2,*) '!Sextupole BEAM1'
      write(2,'(a15,1x,e20.12,a2)') 'KSF.B1      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'KSD.B1      := ',0.,' ;'

      write(2,*)
      write(2,*) '!Strong sextupoles of sectors 81/12/45/56'
      if (arc_squeeze<0.1) then
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A81B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A12B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A45B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A56B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A81B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A12B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A45B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A56B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      else
      write(2,'(a15,1x,e20.12,a2)')'KSF1.A81B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSF1.A12B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSF1.A45B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSF1.A56B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD2.A81B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD2.A12B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD2.A45B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD2.A56B1  := ',ak( i),' ;'
      i=i+5
      endif

      write(2,*)
      write(2,*) '!Weak sextupoles of sectors 81/12/45/56'
      if (arc_squeeze<0.1) then
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A81B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A12B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A45B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A56B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A81B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A12B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A45B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A56B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      else
      write(2,'(a15,1x,e20.12,a2)')'KSF2.A81B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSF2.A12B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSF2.A45B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSF2.A56B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD1.A81B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD1.A12B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD1.A45B1  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD1.A56B1  := ',ak( i),' ;'
      i=i+5
      endif

      write(2,*)
      write(2,*) '!Weak sextupoles of sectors 78/23/34/67'
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A78B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A78B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A23B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A23B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A34B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A34B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A67B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A67B1  := ',ak( i),' +KSF.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A78B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A78B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A23B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A23B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A34B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A34B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A67B1  := ',ak( i),' +KSD.B1;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A67B1  := ',ak( i),' +KSD.B1;'
      i=i+5

      write(2,*)
      write(2,*) '!Sextupole BEAM2'
      write(2,'(a15,1x,e20.12,a2)') 'KSF.B2      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'KSD.B2      := ',0.,' ;'

      write(2,*)
      write(2,*) '!Strong sextupoles of sectors 81/12/45/56'
      if (arc_squeeze<0.1) then
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A81B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A12B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A45B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A56B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A81B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A12B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A45B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A56B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      else
      write(2,'(a15,1x,e20.12,a2)')'KSF2.A81B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSF2.A12B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSF2.A45B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSF2.A56B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD1.A81B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD1.A12B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD1.A45B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD1.A56B2  := ',ak( i),' ;'
      i=i+5
      endif

      write(2,*)
      write(2,*) '!Weak sextupoles of sectors 81/12/45/56'
      if (arc_squeeze<0.1) then
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A81B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A12B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A45B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A56B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A81B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A12B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A45B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A56B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      else
      write(2,'(a15,1x,e20.12,a2)')'KSF1.A81B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSF1.A12B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSF1.A45B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSF1.A56B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD2.A81B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD2.A12B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD2.A45B2  := ',ak( i),' ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a2)')'KSD2.A56B2  := ',ak( i),' ;'
      i=i+5
      endif

      write(2,*)
      write(2,*) '!Weak sextupoles of sectors 78/23/34/67'
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A78B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A78B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A23B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A23B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A34B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A34B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF1.A67B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSF2.A67B2  := ',ak( i),' +KSF.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A78B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A78B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A23B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A23B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A34B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A34B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD1.A67B2  := ',ak( i),' +KSD.B2;'
      i=i+5
      write(2,'(a15,1x,e20.12,a11)')'KSD2.A67B2  := ',ak( i),' +KSD.B2;'
      i=i+5

      i=i+121+15

      else

      write(2,*)
      write(2,*) '!QTF/QTD BEAM1'
      write(2,'(a15,1x,e20.12,a2)') 'dQx.b1      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'dQy.b1      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'dQx.b1_sq   := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'dQy.b1_sq   := ',0.,' ;'
      write(2,*)
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A81B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A12B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A45B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A56B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A81B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A12B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A45B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A56B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A78B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A23B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A34B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A67B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A78B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A23B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A34B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A67B1  := ',ak( i),' + (',ak(i+1),') * dQx.b1 + (',
     +                                ak(i+2),') * dQy.b1 + (',
     +                                ak(i+3),') * dQx.b1_sq + (',
     +                                ak(i+4),') * dQy.b1_sq  ;'
      i=i+5

      write(2,*)
      write(2,*) '!QTF/QTD BEAM2'
      write(2,'(a15,1x,e20.12,a2)') 'dQx.b2      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'dQy.b2      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'dQx.b2_sq   := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'dQy.b2_sq   := ',0.,' ;'
      write(2,*)
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A81B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A12B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A45B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A56B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A81B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A12B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A45B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A56B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A78B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A23B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A34B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTF.A67B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A78B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A23B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A34B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a14,e19.12,
     +          a14,e19.12,a17,e19.12,a16)')
     +'KQTD.A67B2  := ',ak( i),' + (',ak(i+1),') * dQx.b2 + (',
     +                                ak(i+2),') * dQy.b2 + (',
     +                                ak(i+3),') * dQx.b2_sq + (',
     +                                ak(i+4),') * dQy.b2_sq  ;'
      i=i+5

      write(2,*)
      write(2,*) '!Sextupole BEAM1'
      write(2,'(a15,1x,e20.12,a2)') 'dQpx.b1      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'dQpy.b1      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'dQpx.b1_sq   := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'dQpy.b1_sq   := ',0.,' ;'
      write(2,*)
      write(2,*) '!Strong sextupoles of sectors 81/12/45/56'
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A81B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A12B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A45B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A56B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A81B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A12B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A45B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A56B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,*)
      write(2,*) '!Weak sextupoles of sectors 81/12/45/56'
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A81B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A12B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A45B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A56B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A81B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A12B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A45B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A56B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,*)
      write(2,*) '!Weak sextupoles of sectors 78/23/34/67'
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A78B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A78B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A23B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A23B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
       write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A34B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A34B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A67B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A67B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A78B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A78B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A23B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A23B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
       write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A34B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A34B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A67B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A67B1  := ',ak( i),' + (',ak(i+1),') * dQpx.b1 + (',
     +                                ak(i+2),') * dQpy.b1 + (',
     +                                ak(i+3),') * dQpx.b1_sq + (',
     +                                ak(i+4),') * dQpy.b1_sq  ;'
      i=i+5

      write(2,*)
      write(2,*) '!Sextupole BEAM2'
      write(2,'(a15,1x,e20.12,a2)') 'dQpx.b2      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'dQpy.b2      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'dQpx.b2_sq   := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'dQpy.b2_sq   := ',0.,' ;'
      write(2,*)
      write(2,*) '!Strong sextupoles of sectors 81/12/45/56'
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A81B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A12B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A45B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A56B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A81B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A12B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A45B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A56B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,*)
      write(2,*) '!Weak sextupoles of sectors 81/12/45/56'
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A81B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A12B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A45B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A56B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A81B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A12B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A45B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A56B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,*)
      write(2,*) '!Weak sextupoles of sectors 78/23/34/67'
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A78B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A78B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A23B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A23B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
       write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A34B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A34B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF1.A67B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSF2.A67B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A78B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A78B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A23B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A23B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
       write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A34B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A34B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD1.A67B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5
      write(2,'(a15,1x,e20.12,a4,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KSD2.A67B2  := ',ak( i),' + (',ak(i+1),') * dQpx.b2 + (',
     +                                ak(i+2),') * dQpy.b2 + (',
     +                                ak(i+3),') * dQpx.b2_sq + (',
     +                                ak(i+4),') * dQpy.b2_sq  ;'
      i=i+5

      write(2,*)
      write(2,*) '!MQS BEAM1'
      write(2,'(a15,1x,e20.12,a2)') 'CMRS.b1      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'CMIS.b1      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'CMRS.b1_sq   := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'CMIS.b1_sq   := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'ona2_b1      := ',0.,' ;'
      write(2,*)
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.R1B1   :=  (',ak(  i),') * ona2_b1 + (',
     +                    ak(i+1),') * CMRS.b1 + (',
     +                    ak(i+2),') * CMIS.b1 + (',
     +                    ak(i+3),') * CMRS.b1_sq + (',
     +                    ak(i+4),') * CMIS.b1_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.L2B1   :=  (',ak(  i),') * ona2_b1 + (',
     +                    ak(i+1),') * CMRS.b1 + (',
     +                    ak(i+2),') * CMIS.b1 + (',
     +                    ak(i+3),') * CMRS.b1_sq + (',
     +                    ak(i+4),') * CMIS.b1_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KQS.A23B1   :=  (',ak(  i),') * ona2_b1 + (',
     +                    ak(i+1),') * CMRS.b1 + (',
     +                    ak(i+2),') * CMIS.b1 + (',
     +                    ak(i+3),') * CMRS.b1_sq + (',
     +                    ak(i+4),') * CMIS.b1_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.R3B1   :=  (',ak(  i),') * ona2_b1 + (',
     +                    ak(i+1),') * CMRS.b1 + (',
     +                    ak(i+2),') * CMIS.b1 + (',
     +                    ak(i+3),') * CMRS.b1_sq + (',
     +                    ak(i+4),') * CMIS.b1_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.L4B1   :=  (',ak(  i),') * ona2_b1 + (',
     +                    ak(i+1),') * CMRS.b1 + (',
     +                    ak(i+2),') * CMIS.b1 + (',
     +                    ak(i+3),') * CMRS.b1_sq + (',
     +                    ak(i+4),') * CMIS.b1_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KQS.A45B1   :=  (',ak(  i),') * ona2_b1 + (',
     +                    ak(i+1),') * CMRS.b1 + (',
     +                    ak(i+2),') * CMIS.b1 + (',
     +                    ak(i+3),') * CMRS.b1_sq + (',
     +                    ak(i+4),') * CMIS.b1_sq  ;'
      i=i+5
       write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.R5B1   :=  (',ak(  i),') * ona2_b1 + (',
     +                    ak(i+1),') * CMRS.b1 + (',
     +                    ak(i+2),') * CMIS.b1 + (',
     +                    ak(i+3),') * CMRS.b1_sq + (',
     +                    ak(i+4),') * CMIS.b1_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.L6B1   :=  (',ak(  i),') * ona2_b1 + (',
     +                    ak(i+1),') * CMRS.b1 + (',
     +                    ak(i+2),') * CMIS.b1 + (',
     +                    ak(i+3),') * CMRS.b1_sq + (',
     +                    ak(i+4),') * CMIS.b1_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KQS.A67B1   :=  (',ak(  i),') * ona2_b1 + (',
     +                    ak(i+1),') * CMRS.b1 + (',
     +                    ak(i+2),') * CMIS.b1 + (',
     +                    ak(i+3),') * CMRS.b1_sq + (',
     +                    ak(i+4),') * CMIS.b1_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.R7B1   :=  (',ak(  i),') * ona2_b1 + (',
     +                    ak(i+1),') * CMRS.b1 + (',
     +                    ak(i+2),') * CMIS.b1 + (',
     +                    ak(i+3),') * CMRS.b1_sq + (',
     +                    ak(i+4),') * CMIS.b1_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.L8B1   :=  (',ak(  i),') * ona2_b1 + (',
     +                    ak(i+1),') * CMRS.b1 + (',
     +                    ak(i+2),') * CMIS.b1 + (',
     +                    ak(i+3),') * CMRS.b1_sq + (',
     +                    ak(i+4),') * CMIS.b1_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KQS.A81B1   :=  (',ak(  i),') * ona2_b1 + (',
     +                    ak(i+1),') * CMRS.b1 + (',
     +                    ak(i+2),') * CMIS.b1 + (',
     +                    ak(i+3),') * CMRS.b1_sq + (',
     +                    ak(i+4),') * CMIS.b1_sq  ;'
      i=i+5

      write(2,*)
      write(2,*) '!MQS BEAM2'
      write(2,'(a15,1x,e20.12,a2)') 'CMRS.b2      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'CMIS.b2      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'CMRS.b2_sq   := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'CMIS.b2_sq   := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'ona2_b2      := ',0.,' ;'
      write(2,*)
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KQS.A12B2   :=  (',ak(  i),') * ona2_b2 + (',
     +                    ak(i+1),') * CMRS.b2 + (',
     +                    ak(i+2),') * CMIS.b2 + (',
     +                    ak(i+3),') * CMRS.b2_sq + (',
     +                    ak(i+4),') * CMIS.b2_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.R2B2   :=  (',ak(  i),') * ona2_b2 + (',
     +                    ak(i+1),') * CMRS.b2 + (',
     +                    ak(i+2),') * CMIS.b2 + (',
     +                    ak(i+3),') * CMRS.b2_sq + (',
     +                    ak(i+4),') * CMIS.b2_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.L3B2   :=  (',ak(  i),') * ona2_b2 + (',
     +                    ak(i+1),') * CMRS.b2 + (',
     +                    ak(i+2),') * CMIS.b2 + (',
     +                    ak(i+3),') * CMRS.b2_sq + (',
     +                    ak(i+4),') * CMIS.b2_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KQS.A34B2   :=  (',ak(  i),') * ona2_b2 + (',
     +                    ak(i+1),') * CMRS.b2 + (',
     +                    ak(i+2),') * CMIS.b2 + (',
     +                    ak(i+3),') * CMRS.b2_sq + (',
     +                    ak(i+4),') * CMIS.b2_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.R4B2   :=  (',ak(  i),') * ona2_b2 + (',
     +                    ak(i+1),') * CMRS.b2 + (',
     +                    ak(i+2),') * CMIS.b2 + (',
     +                    ak(i+3),') * CMRS.b2_sq + (',
     +                    ak(i+4),') * CMIS.b2_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.L5B2   :=  (',ak(  i),') * ona2_b2 + (',
     +                    ak(i+1),') * CMRS.b2 + (',
     +                    ak(i+2),') * CMIS.b2 + (',
     +                    ak(i+3),') * CMRS.b2_sq + (',
     +                    ak(i+4),') * CMIS.b2_sq  ;'
      i=i+5
       write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KQS.A56B2   :=  (',ak(  i),') * ona2_b2 + (',
     +                    ak(i+1),') * CMRS.b2 + (',
     +                    ak(i+2),') * CMIS.b2 + (',
     +                    ak(i+3),') * CMRS.b2_sq + (',
     +                    ak(i+4),') * CMIS.b2_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.R6B2   :=  (',ak(  i),') * ona2_b2 + (',
     +                    ak(i+1),') * CMRS.b2 + (',
     +                    ak(i+2),') * CMIS.b2 + (',
     +                    ak(i+3),') * CMRS.b2_sq + (',
     +                    ak(i+4),') * CMIS.b2_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.L7B2   :=  (',ak(  i),') * ona2_b2 + (',
     +                    ak(i+1),') * CMRS.b2 + (',
     +                    ak(i+2),') * CMIS.b2 + (',
     +                    ak(i+3),') * CMRS.b2_sq + (',
     +                    ak(i+4),') * CMIS.b2_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +'KQS.A78B2   :=  (',ak(  i),') * ona2_b2 + (',
     +                    ak(i+1),') * CMRS.b2 + (',
     +                    ak(i+2),') * CMIS.b2 + (',
     +                    ak(i+3),') * CMRS.b2_sq + (',
     +                    ak(i+4),') * CMIS.b2_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.R8B2   :=  (',ak(  i),') * ona2_b2 + (',
     +                    ak(i+1),') * CMRS.b2 + (',
     +                    ak(i+2),') * CMIS.b2 + (',
     +                    ak(i+3),') * CMRS.b2_sq + (',
     +                    ak(i+4),') * CMIS.b2_sq  ;'
      i=i+5
      write(2,'(a17,1x,e20.12,a15,e19.12,a15,e19.12,
     +          a15,e19.12,a18,e19.12,a17)')
     +' KQS.L1B2   :=  (',ak(  i),') * ona2_b2 + (',
     +                    ak(i+1),') * CMRS.b2 + (',
     +                    ak(i+2),') * CMIS.b2 + (',
     +                    ak(i+3),') * CMRS.b2_sq + (',
     +                    ak(i+4),') * CMIS.b2_sq  ;'
      i=i+5

      write(2,*)
      write(2,*) '!MSS BEAM1'
      write(2,'(a15,1x,e20.12,a2)') 'ona3_b1      := ',0.,' ;'
      write(2,*)
      write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A12B1   :=  (',ak(  i),') * ona3_b1 ;'
      i=i+1
      write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A23B1   :=  (',ak(  i),') * ona3_b1 ;'
      i=i+1
      write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A34B1   :=  (',ak(  i),') * ona3_b1 ;'
      i=i+1
      write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A45B1   :=  (',ak(  i),') * ona3_b1 ;'
      i=i+1
       write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A56B1   :=  (',ak(  i),') * ona3_b1 ;'
      i=i+1
      write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A67B1   :=  (',ak(  i),') * ona3_b1 ;'
      i=i+1
      write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A78B1   :=  (',ak(  i),') * ona3_b1 ;'
      i=i+1
      write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A81B1   :=  (',ak(  i),') * ona3_b1 ;'
      i=i+1

      write(2,*)
      write(2,*) '!MSS BEAM2'
      write(2,'(a15,1x,e20.12,a2)') 'ona3_b2      := ',0.,' ;'
      write(2,*)
      write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A12B2   :=  (',ak(  i),') * ona3_b2 ;'
      i=i+1
      write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A23B2   :=  (',ak(  i),') * ona3_b2 ;'
      i=i+1
      write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A34B2   :=  (',ak(  i),') * ona3_b2 ;'
      i=i+1
      write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A45B2   :=  (',ak(  i),') * ona3_b2 ;'
      i=i+1
       write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A56B2   :=  (',ak(  i),') * ona3_b2 ;'
      i=i+1
      write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A67B2   :=  (',ak(  i),') * ona3_b2 ;'
      i=i+1
      write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A78B2   :=  (',ak(  i),') * ona3_b2 ;'
      i=i+1
      write(2,'(a17,1x,e20.12,a13)')
     +'KSS.A81B2   :=  (',ak(  i),') * ona3_b2 ;'
      i=i+1

      endif

      write(2,*)
      write(2,*) '!OF/OD BEAM1'
      write(2,'(a15,1x,e20.12,a2)') 'KOF.B1      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'KOD.B1      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A12B1   := ',ak( i),'*ON_QPP + KOF.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A23B1   := ',ak( i),'*ON_QPP + KOF.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A34B1   := ',ak( i),'*ON_QPP + KOF.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A45B1   := ',ak( i),'*ON_QPP + KOF.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A56B1   := ',ak( i),'*ON_QPP + KOF.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A67B1   := ',ak( i),'*ON_QPP + KOF.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A78B1   := ',ak( i),'*ON_QPP + KOF.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A81B1   := ',ak( i),'*ON_QPP + KOF.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A12B1   := ',ak( i),'*ON_QPP + KOD.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A23B1   := ',ak( i),'*ON_QPP + KOD.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A34B1   := ',ak( i),'*ON_QPP + KOD.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A45B1   := ',ak( i),'*ON_QPP + KOD.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A56B1   := ',ak( i),'*ON_QPP + KOD.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A67B1   := ',ak( i),'*ON_QPP + KOD.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A78B1   := ',ak( i),'*ON_QPP + KOD.B1;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A81B1   := ',ak( i),'*ON_QPP + KOD.B1;'
      i=i+1

      write(2,*)
      write(2,*) '!OF/OD BEAM2'
      write(2,'(a15,1x,e20.12,a2)') 'KOF.B2      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a2)') 'KOD.B2      := ',0.,' ;'
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A12B2   := ',ak( i),'*ON_QPP + KOF.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A23B2   := ',ak( i),'*ON_QPP + KOF.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A34B2   := ',ak( i),'*ON_QPP + KOF.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A45B2   := ',ak( i),'*ON_QPP + KOF.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A56B2   := ',ak( i),'*ON_QPP + KOF.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A67B2   := ',ak( i),'*ON_QPP + KOF.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A78B2   := ',ak( i),'*ON_QPP + KOF.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOF.A81B2   := ',ak( i),'*ON_QPP + KOF.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A12B2   := ',ak( i),'*ON_QPP + KOD.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A23B2   := ',ak( i),'*ON_QPP + KOD.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A34B2   := ',ak( i),'*ON_QPP + KOD.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A45B2   := ',ak( i),'*ON_QPP + KOD.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A56B2   := ',ak( i),'*ON_QPP + KOD.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A67B2   := ',ak( i),'*ON_QPP + KOD.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A78B2   := ',ak( i),'*ON_QPP + KOD.B2;'
      i=i+1
      write(2,'(a15,1x,e20.12,a18)')
     +'KOD.A81B2   := ',ak( i),'*ON_QPP + KOD.B2;'
      i=i+1

      write(2,*)
      write(2,*) '!! MCB-beam1 for spurious dispersion correction'

      write(2,*)
      write(2,*) '!! MCB in sector 81 and 12'
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.r8b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.r8b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh14.r8b1  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh16.r8b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh16.r8b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh16.r8b1  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.l1b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.l1b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh14.l1b1  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh12.l1b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh12.l1b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh12.l1b1  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.r1b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.r1b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh13.r1b1  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.r1b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.r1b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh15.r1b1  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.l2b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.l2b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh15.l2b1  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.l2b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.l2b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh13.l2b1  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.r8b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.r8b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv13.r8b1  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.r8b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.r8b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv15.r8b1  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.l1b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.l1b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv15.l1b1  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.l1b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.l1b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv13.l1b1  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv12.r1b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv12.r1b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv12.r1b1  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.r1b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.r1b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv14.r1b1  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv16.l2b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv16.l2b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv16.l2b1  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.l2b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.l2b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv14.l2b1  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'

      write(2,*)
      write(2,*) '!! MCB in sector 45 and 56'
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.r4b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.r4b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh14.r4b1  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh16.r4b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh16.r4b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh16.r4b1  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.l5b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.l5b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh14.l5b1  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh12.l5b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh12.l5b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh12.l5b1  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.r5b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.r5b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh13.r5b1  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.r5b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.r5b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh15.r5b1  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.l6b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.l6b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh15.l6b1  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.l6b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.l6b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh13.l6b1  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.r4b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.r4b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv13.r4b1  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.r4b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.r4b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv15.r4b1  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.l5b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.l5b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv15.l5b1  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.l5b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.l5b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv13.l5b1  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv12.r5b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv12.r5b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv12.r5b1  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.r5b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.r5b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv14.r5b1  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv16.l6b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv16.l6b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv16.l6b1  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.l6b1x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.l6b1s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv14.l6b1  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep5)*ON_DISP ;'

      write(2,*)
      write(2,*) '!! MCB-beam2 for spurious dispersion correction'

      write(2,*)
      write(2,*) '!! MCB in sector 81 and 12'
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.r8b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.r8b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh13.r8b2  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.r8b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.r8b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh15.r8b2  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.l1b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.l1b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh15.l1b2  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.l1b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.l1b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh13.l1b2  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh12.r1b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh12.r1b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh12.r1b2  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.r1b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.r1b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh14.r1b2  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh16.l2b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh16.l2b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh16.l2b2  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.l2b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.l2b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh14.l2b2  := ((',ak(i-2),')*cos(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*sin(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.r8b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.r8b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv14.r8b2  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv16.r8b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv16.r8b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv16.r8b2  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.l1b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.l1b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv14.l1b2  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv12.l1b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv12.l1b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv12.l1b2  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.r1b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.r1b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv13.r1b2  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.r1b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.r1b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv15.r1b2  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.l2b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.l2b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv15.l2b2  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.l2b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.l2b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv13.l2b2  := ((',ak(i-2),')*sin(phi_IR1*twopi/360.)*on_x1+(',
     +           ak(i-1),')*cos(phi_IR1*twopi/360.)*on_sep1)*ON_DISP ;'

      write(2,*)
      write(2,*) '!! MCB in sector 45 and 56'
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.r4b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.r4b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh13.r4b2  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.r4b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.r4b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh15.r4b2  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.l5b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh15.l5b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh15.l5b2  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.l5b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh13.l5b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh13.l5b2  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh12.r5b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh12.r5b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh12.r5b2  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.r5b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.r5b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh14.r5b2  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh16.l6b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh16.l6b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh16.l6b2  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.l6b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbh14.l6b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbh14.l6b2  := ((',ak(i-2),')*cos(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*sin(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.r4b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.r4b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv14.r4b2  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv16.r4b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv16.r4b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv16.r4b2  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.l5b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv14.l5b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv14.l5b2  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv12.l5b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv12.l5b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv12.l5b2  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.r5b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.r5b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv13.r5b2  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.r5b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.r5b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv15.r5b2  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.l6b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv15.l6b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv15.l6b2  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.l6b2x  := ',ak( i),';'
      i=i+1
      write(2,'(a17,1x,e20.12,a2)') 'acbv13.l6b2s  := ',ak( i),';'
      i=i+1
      write(2,'(a18,1x,e20.12,a35,e20.12,1x,a45)')
     +'acbv13.l6b2  := ((',ak(i-2),')*sin(phi_IR5*twopi/360.)*on_x5+(',
     +           ak(i-1),')*cos(phi_IR5*twopi/360.)*on_sep5)*ON_DISP ;'

      if(on_check.gt.0.1) then
      write(2,*)
      write(2,*) '!****OPTICS SUMMARY****'

      write(2,*)
      write(2,*) '!Tune and Chroma'
      write(2,'(4(a6,f11.6,a1,2x))')
     +'Qxb1 =',ak(i),';','Qyb1 =',ak(i+1),';',
     +'Qpxb1=',ak(i+2),';','Qpyb1=',ak(i+3),';'
      i=i+4
      write(2,'(4(a6,f11.6,a1,2x))')
     +'Qxb2 =',ak(i),';','Qyb2 =',ak(i+1),';',
     +'Qpxb2=',ak(i+2),';','Qpyb2=',ak(i+3),';'
      i=i+4

      write(2,*)
      write(2,*) '!IR Optics summary (phase, twiss param., dispersion)'
      write(2,'(2(a9,f10.7,a1,2x),4(a11,f10.7,a1,2x),
     +                               6(a10,f12.6,a1,2x))')
     +'muxIP1b1=',ak(i),';','muyIP1b1=',ak(i+1),';',
     +'muxIP1b1_L=',ak(i+2),';','muyIP1b1_L=',ak(i+3),';',
     +'muxIP1b1_R=',ak(i+4),';','muyIP1b1_R=',ak(i+5),';',
     +'betxIP1b1=',ak(i+6),';','betyIP1b1=',ak(i+7),';',
     +'alfxIP1b1=',ak(i+8),';','alfyIP1b1=',ak(i+9),';',
     +'dxIP1b1=',ak(i+10),';','dpxIP1b1=',ak(i+11),';'
      i=i+12
      write(2,'(2(a9,f10.7,a1,2x),4(a11,f10.7,a1,2x),
     +                               6(a10,f12.6,a1,2x))')
     +'muxIP1b2=',ak(i),';','muyIP1b2=',ak(i+1),';',
     +'muxIP1b2_L=',ak(i+2),';','muyIP1b2_L=',ak(i+3),';',
     +'muxIP1b2_R=',ak(i+4),';','muyIP1b2_R=',ak(i+5),';',
     +'betxIP1b2=',ak(i+6),';','betyIP1b2=',ak(i+7),';',
     +'alfxIP1b2=',ak(i+8),';','alfyIP1b2=',ak(i+9),';',
     +'dxIP1b2=',ak(i+10),';','dpxIP1b2=',ak(i+11),';'
      i=i+12
      write(2,'(2(a9,f10.7,a1,2x),4(a11,f10.7,a1,2x),
     +                               6(a10,f12.6,a1,2x))')
     +'muxIP5b1=',ak(i),';','muyIP5b1=',ak(i+1),';',
     +'muxIP5b1_L=',ak(i+2),';','muyIP5b1_L=',ak(i+3),';',
     +'muxIP5b1_R=',ak(i+4),';','muyIP5b1_R=',ak(i+5),';',
     +'betxIP5b1=',ak(i+6),';','betyIP5b1=',ak(i+7),';',
     +'alfxIP5b1=',ak(i+8),';','alfyIP5b1=',ak(i+9),';',
     +'dxIP5b1=',ak(i+10),';','dpxIP5b1=',ak(i+11),';'
      i=i+12
      write(2,'(2(a9,f10.7,a1,2x),4(a11,f10.7,a1,2x),
     +                               6(a10,f12.6,a1,2x))')
     +'muxIP5b2=',ak(i),';','muyIP5b2=',ak(i+1),';',
     +'muxIP5b2_L=',ak(i+2),';','muyIP5b2_L=',ak(i+3),';',
     +'muxIP5b2_R=',ak(i+4),';','muyIP5b2_R=',ak(i+5),';',
     +'betxIP5b2=',ak(i+6),';','betyIP5b2=',ak(i+7),';',
     +'alfxIP5b2=',ak(i+8),';','alfyIP5b2=',ak(i+9),';',
     +'dxIP5b2=',ak(i+10),';','dpxIP5b2=',ak(i+11),';'
      i=i+12
      write(2,'(2(a9,f10.7,a1,2x),6(a10,f12.6,a1,2x))')
     +'muxIP2b1=',ak(i),';','muyIP2b1=',ak(i+1),';',
     +'betxIP2b1=',ak(i+2),';','betyIP2b1=',ak(i+3),';',
     +'alfxIP2b1=',ak(i+4),';','alfyIP2b1=',ak(i+5),';',
     +'dxIP2b1=',ak(i+6),';','dpxIP2b1=',ak(i+7),';'
      i=i+8
      write(2,'(2(a9,f10.7,a1,2x),6(a10,f12.6,a1,2x))')
     +'muxIP2b2=',ak(i),';','muyIP2b2=',ak(i+1),';',
     +'betxIP2b2=',ak(i+2),';','betyIP2b2=',ak(i+3),';',
     +'alfxIP2b2=',ak(i+4),';','alfyIP2b2=',ak(i+5),';',
     +'dxIP2b2=',ak(i+6),';','dpxIP2b2=',ak(i+7),';'
      i=i+8
      write(2,'(2(a9,f10.7,a1,2x),6(a10,f12.6,a1,2x))')
     +'muxIP8b1=',ak(i),';','muyIP8b1=',ak(i+1),';',
     +'betxIP8b1=',ak(i+2),';','betyIP8b1=',ak(i+3),';',
     +'alfxIP8b1=',ak(i+4),';','alfyIP8b1=',ak(i+5),';',
     +'dxIP8b1=',ak(i+6),';','dpxIP8b1=',ak(i+7),';'
      i=i+8
      write(2,'(2(a9,f10.7,a1,2x),6(a10,f12.6,a1,2x))')
     +'muxIP8b2=',ak(i),';','muyIP8b2=',ak(i+1),';',
     +'betxIP8b2=',ak(i+2),';','betyIP8b2=',ak(i+3),';',
     +'alfxIP8b2=',ak(i+4),';','alfyIP8b2=',ak(i+5),';',
     +'dxIP8b2=',ak(i+6),';','dpxIP8b2=',ak(i+7),';'
      i=i+8
      write(2,'(2(a9,f10.7,a1,2x),6(a10,f12.6,a1,2x))')
     +'muxIP4b1=',ak(i),';','muyIP4b1=',ak(i+1),';',
     +'betxIP4b1=',ak(i+2),';','betyIP4b1=',ak(i+3),';',
     +'alfxIP4b1=',ak(i+4),';','alfyIP4b1=',ak(i+5),';',
     +'dxIP4b1=',ak(i+6),';','dpxIP4b1=',ak(i+7),';'
      i=i+8
      write(2,'(2(a9,f10.7,a1,2x),6(a10,f12.6,a1,2x))')
     +'muxIP4b2=',ak(i),';','muyIP4b2=',ak(i+1),';',
     +'betxIP4b2=',ak(i+2),';','betyIP4b2=',ak(i+3),';',
     +'alfxIP4b2=',ak(i+4),';','alfyIP4b2=',ak(i+5),';',
     +'dxIP4b2=',ak(i+6),';','dpxIP4b2=',ak(i+7),';'
      i=i+8
      write(2,'(2(a9,f10.7,a1,2x),6(a10,f12.6,a1,2x))')
     +'muxIP6b1=',ak(i),';','muyIP6b1=',ak(i+1),';',
     +'betxIP6b1=',ak(i+2),';','betyIP6b1=',ak(i+3),';',
     +'alfxIP6b1=',ak(i+4),';','alfyIP6b1=',ak(i+5),';',
     +'dxIP6b1=',ak(i+6),';','dpxIP6b1=',ak(i+7),';'
      i=i+8
      write(2,'(2(a9,f10.7,a1,2x),6(a10,f12.6,a1,2x))')
     +'muxIP6b2=',ak(i),';','muyIP6b2=',ak(i+1),';',
     +'betxIP6b2=',ak(i+2),';','betyIP6b2=',ak(i+3),';',
     +'alfxIP6b2=',ak(i+4),';','alfyIP6b2=',ak(i+5),';',
     +'dxIP6b2=',ak(i+6),';','dpxIP6b2=',ak(i+7),';'
      i=i+8
      write(2,'(2(a9,f10.7,a1,2x),6(a10,f12.6,a1,2x))')
     +'muxIP3b1=',ak(i),';','muyIP3b1=',ak(i+1),';',
     +'betxIP3b1=',ak(i+2),';','betyIP3b1=',ak(i+3),';',
     +'alfxIP3b1=',ak(i+4),';','alfyIP3b1=',ak(i+5),';',
     +'dxIP3b1=',ak(i+6),';','dpxIP3b1=',ak(i+7),';'
      i=i+8
      write(2,'(2(a9,f10.7,a1,2x),6(a10,f12.6,a1,2x))')
     +'muxIP3b2=',ak(i),';','muyIP3b2=',ak(i+1),';',
     +'betxIP3b2=',ak(i+2),';','betyIP3b2=',ak(i+3),';',
     +'alfxIP3b2=',ak(i+4),';','alfyIP3b2=',ak(i+5),';',
     +'dxIP3b2=',ak(i+6),';','dpxIP3b2=',ak(i+7),';'
      i=i+8
      write(2,'(2(a9,f10.7,a1,2x),6(a10,f12.6,a1,2x))')
     +'muxIP7b1=',ak(i),';','muyIP7b1=',ak(i+1),';',
     +'betxIP7b1=',ak(i+2),';','betyIP7b1=',ak(i+3),';',
     +'alfxIP7b1=',ak(i+4),';','alfyIP7b1=',ak(i+5),';',
     +'dxIP7b1=',ak(i+6),';','dpxIP7b1=',ak(i+7),';'
      i=i+8
      write(2,'(2(a9,f10.7,a1,2x),6(a10,f12.6,a1,2x))')
     +'muxIP7b2=',ak(i),';','muyIP7b2=',ak(i+1),';',
     +'betxIP7b2=',ak(i+2),';','betyIP7b2=',ak(i+3),';',
     +'alfxIP7b2=',ak(i+4),';','alfyIP7b2=',ak(i+5),';',
     +'dxIP7b2=',ak(i+6),';','dpxIP7b2=',ak(i+7),';'
      i=i+8

      write(2,*)
      write(2,*) '!Xscheme summary in IR1, IR2,IR5 and IR8'
      write(2,'(2(a7,f13.10,a1,2x),2(a8,f13.10,a1,2x))')
     +'xIP1b1=',ak(i),';','yIP1b1=',ak(i+1),';',
     +'pxIP1b1=',ak(i+2),';','pyIP1b1=',ak(i+3),';'
      i=i+4
      write(2,'(2(a7,f13.10,a1,2x),2(a8,f13.10,a1,2x))')
     +'xIP1b2=',ak(i),';','yIP1b2=',ak(i+1),';',
     +'pxIP1b2=',ak(i+2),';','pyIP1b2=',ak(i+3),';'
      i=i+4
      write(2,'(2(a7,f13.10,a1,2x),2(a8,f13.10,a1,2x))')
     +'xIP2b1=',ak(i),';','yIP2b1=',ak(i+1),';',
     +'pxIP2b1=',ak(i+2),';','pyIP2b1=',ak(i+3),';'
      i=i+4
      write(2,'(2(a7,f13.10,a1,2x),2(a8,f13.10,a1,2x))')
     +'xIP2b2=',ak(i),';','yIP2b2=',ak(i+1),';',
     +'pxIP2b2=',ak(i+2),';','pyIP2b2=',ak(i+3),';'
      i=i+4
      write(2,'(2(a7,f13.10,a1,2x),2(a8,f13.10,a1,2x))')
     +'xIP5b1=',ak(i),';','yIP5b1=',ak(i+1),';',
     +'pxIP5b1=',ak(i+2),';','pyIP5b1=',ak(i+3),';'
      i=i+4
      write(2,'(2(a7,f13.10,a1,2x),2(a8,f13.10,a1,2x))')
     +'xIP5b2=',ak(i),';','yIP5b2=',ak(i+1),';',
     +'pxIP5b2=',ak(i+2),';','pyIP5b2=',ak(i+3),';'
      i=i+4
      write(2,'(2(a7,f13.10,a1,2x),2(a8,f13.10,a1,2x))')
     +'xIP8b1=',ak(i),';','yIP8b1=',ak(i+1),';',
     +'pxIP8b1=',ak(i+2),';','pyIP8b1=',ak(i+3),';'
      i=i+4
      write(2,'(2(a7,f13.10,a1,2x),2(a8,f13.10,a1,2x))')
     +'xIP8b2=',ak(i),';','yIP8b2=',ak(i+1),';',
     +'pxIP8b2=',ak(i+2),';','pyIP8b2=',ak(i+3),';'
      i=i+4

      write(2,*)
      write(2,*) '!Arc Optics summary'
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell81b1=',ak(i),';','muycell81b1=',ak(i+1),';',
     +'mux81b1=',ak(i+2),';','muy81b1=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell45b1=',ak(i),';','muycell45b1=',ak(i+1),';',
     +'mux45b1=',ak(i+2),';','muy45b1=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell12b2=',ak(i),';','muycell12b2=',ak(i+1),';',
     +'mux12b2=',ak(i+2),';','muy12b2=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell56b2=',ak(i),';','muycell56b2=',ak(i+1),';',
     +'mux56b2=',ak(i+2),';','muy56b2=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell12b1=',ak(i),';','muycell12b1=',ak(i+1),';',
     +'mux12b1=',ak(i+2),';','muy12b1=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell56b1=',ak(i),';','muycell56b1=',ak(i+1),';',
     +'mux56b1=',ak(i+2),';','muy56b1=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell81b2=',ak(i),';','muycell81b2=',ak(i+1),';',
     +'mux81b2=',ak(i+2),';','muy81b2=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell45b2=',ak(i),';','muycell45b2=',ak(i+1),';',
     +'mux45b2=',ak(i+2),';','muy45b2=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell23b1=',ak(i),';','muycell23b1=',ak(i+1),';',
     +'mux23b1=',ak(i+2),';','muy23b1=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell78b2=',ak(i),';','muycell78b2=',ak(i+1),';',
     +'mux78b2=',ak(i+2),';','muy78b2=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell34b1=',ak(i),';','muycell34b1=',ak(i+1),';',
     +'mux34b1=',ak(i+2),';','muy34b1=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell67b2=',ak(i),';','muycell67b2=',ak(i+1),';',
     +'mux67b2=',ak(i+2),';','muy67b2=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell67b1=',ak(i),';','muycell67b1=',ak(i+1),';',
     +'mux67b1=',ak(i+2),';','muy67b1=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell34b2=',ak(i),';','muycell34b2=',ak(i+1),';',
     +'mux34b2=',ak(i+2),';','muy34b2=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell78b1=',ak(i),';','muycell78b1=',ak(i+1),';',
     +'mux78b1=',ak(i+2),';','muy78b1=',ak(i+3),';'
      i=i+4
      write(2,'(2(a12,f10.8,a1,2x),2(a8,f10.7,a1,2x))')
     +'muxcell23b2=',ak(i),';','muycell23b2=',ak(i+1),';',
     +'mux23b2=',ak(i+2),';','muy23b2=',ak(i+3),';'
      i=i+4

      endif

      write(2,*)
      write(2,*)
      write(2,*) 'return ;'


      close(1)
      close(2)

      stop
      end
