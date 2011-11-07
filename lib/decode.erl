%% Author: xiaoshengaya
%% Created: 2010-1-23
%% Description: 解密数据
-module(decode).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([decode/1]).

%%
%% API Functions
%%

decode(0) -> 16#51;
decode(1) -> 16#a1;
decode(2) -> 16#9e;
decode(3) -> 16#b0;
decode(4) -> 16#1e;
decode(5) -> 16#83;
decode(6) -> 16#1c;
decode(7) -> 16#2d;
decode(8) -> 16#e9;
decode(9) -> 16#77;
decode(10) -> 16#3d;
decode(11) -> 16#13;
decode(12) -> 16#93;
decode(13) -> 16#10;
decode(14) -> 16#45;
decode(15) -> 16#ff;
decode(16) -> 16#6d;
decode(17) -> 16#c9;
decode(18) -> 16#20;
decode(19) -> 16#2f;
decode(20) -> 16#1b;
decode(21) -> 16#82;
decode(22) -> 16#1a;
decode(23) -> 16#7d;
decode(24) -> 16#f5;
decode(25) -> 16#cf;
decode(26) -> 16#52;
decode(27) -> 16#a8;
decode(28) -> 16#d2;
decode(29) -> 16#a4;
decode(30) -> 16#b4;
decode(31) -> 16#b;
decode(32) -> 16#31;
decode(33) -> 16#97;
decode(34) -> 16#57;
decode(35) -> 16#19;
decode(36) -> 16#34;
decode(37) -> 16#df;
decode(38) -> 16#5b;
decode(39) -> 16#41;
decode(40) -> 16#58;
decode(41) -> 16#49;
decode(42) -> 16#aa;
decode(43) -> 16#5f;
decode(44) -> 16#a;
decode(45) -> 16#ef;
decode(46) -> 16#88;
decode(47) -> 16#1;
decode(48) -> 16#dc;
decode(49) -> 16#95;
decode(50) -> 16#d4;
decode(51) -> 16#af;
decode(52) -> 16#7b;
decode(53) -> 16#e3;
decode(54) -> 16#11;
decode(55) -> 16#8e;
decode(56) -> 16#9d;
decode(57) -> 16#16;
decode(58) -> 16#61;
decode(59) -> 16#8c;
decode(60) -> 16#84;
decode(61) -> 16#3c;
decode(62) -> 16#1f;
decode(63) -> 16#5a;
decode(64) -> 16#2;
decode(65) -> 16#4f;
decode(66) -> 16#39;
decode(67) -> 16#fe;
decode(68) -> 16#4;
decode(69) -> 16#7;
decode(70) -> 16#5c;
decode(71) -> 16#8b;
decode(72) -> 16#ee;
decode(73) -> 16#66;
decode(74) -> 16#33;
decode(75) -> 16#c4;
decode(76) -> 16#c8;
decode(77) -> 16#59;
decode(78) -> 16#b5;
decode(79) -> 16#5d;
decode(80) -> 16#c2;
decode(81) -> 16#6c;
decode(82) -> 16#f6;
decode(83) -> 16#4d;
decode(84) -> 16#fb;
decode(85) -> 16#ae;
decode(86) -> 16#4a;
decode(87) -> 16#4b;
decode(88) -> 16#f3;
decode(89) -> 16#35;
decode(90) -> 16#2c;
decode(91) -> 16#ca;
decode(92) -> 16#21;
decode(93) -> 16#78;
decode(94) -> 16#3b;
decode(95) -> 16#3;
decode(96) -> 16#fd;
decode(97) -> 16#24;
decode(98) -> 16#bd;
decode(99) -> 16#25;
decode(100) -> 16#37;
decode(101) -> 16#29;
decode(102) -> 16#ac;
decode(103) -> 16#4e;
decode(104) -> 16#f9;
decode(105) -> 16#92;
decode(106) -> 16#3a;
decode(107) -> 16#32;
decode(108) -> 16#4c;
decode(109) -> 16#da;
decode(110) -> 16#6;
decode(111) -> 16#5e;
decode(112) -> 16#0;
decode(113) -> 16#94;
decode(114) -> 16#60;
decode(115) -> 16#ec;
decode(116) -> 16#17;
decode(117) -> 16#98;
decode(118) -> 16#d7;
decode(119) -> 16#3e;
decode(120) -> 16#cb;
decode(121) -> 16#6a;
decode(122) -> 16#a9;
decode(123) -> 16#d9;
decode(124) -> 16#9c;
decode(125) -> 16#bb;
decode(126) -> 16#8;
decode(127) -> 16#8f;
decode(128) -> 16#40;
decode(129) -> 16#a0;
decode(130) -> 16#6f;
decode(131) -> 16#55;
decode(132) -> 16#67;
decode(133) -> 16#87;
decode(134) -> 16#54;
decode(135) -> 16#80;
decode(136) -> 16#b2;
decode(137) -> 16#36;
decode(138) -> 16#47;
decode(139) -> 16#22;
decode(140) -> 16#44;
decode(141) -> 16#63;
decode(142) -> 16#5;
decode(143) -> 16#6b;
decode(144) -> 16#f0;
decode(145) -> 16#f;
decode(146) -> 16#c7;
decode(147) -> 16#90;
decode(148) -> 16#c5;
decode(149) -> 16#65;
decode(150) -> 16#e2;
decode(151) -> 16#64;
decode(152) -> 16#fa;
decode(153) -> 16#d5;
decode(154) -> 16#db;
decode(155) -> 16#12;
decode(156) -> 16#7a;
decode(157) -> 16#e;
decode(158) -> 16#d8;
decode(159) -> 16#7e;
decode(160) -> 16#99;
decode(161) -> 16#d1;
decode(162) -> 16#e8;
decode(163) -> 16#d6;
decode(164) -> 16#86;
decode(165) -> 16#27;
decode(166) -> 16#bf;
decode(167) -> 16#c1;
decode(168) -> 16#6e;
decode(169) -> 16#de;
decode(170) -> 16#9a;
decode(171) -> 16#9;
decode(172) -> 16#d;
decode(173) -> 16#ab;
decode(174) -> 16#e1;
decode(175) -> 16#91;
decode(176) -> 16#56;
decode(177) -> 16#cd;
decode(178) -> 16#b3;
decode(179) -> 16#76;
decode(180) -> 16#c;
decode(181) -> 16#c3;
decode(182) -> 16#d3;
decode(183) -> 16#9f;
decode(184) -> 16#42;
decode(185) -> 16#b6;
decode(186) -> 16#9b;
decode(187) -> 16#e5;
decode(188) -> 16#23;
decode(189) -> 16#a7;
decode(190) -> 16#ad;
decode(191) -> 16#18;
decode(192) -> 16#c6;
decode(193) -> 16#f4;
decode(194) -> 16#b8;
decode(195) -> 16#be;
decode(196) -> 16#15;
decode(197) -> 16#43;
decode(198) -> 16#70;
decode(199) -> 16#e0;
decode(200) -> 16#e7;
decode(201) -> 16#bc;
decode(202) -> 16#f1;
decode(203) -> 16#ba;
decode(204) -> 16#a5;
decode(205) -> 16#a6;
decode(206) -> 16#53;
decode(207) -> 16#75;
decode(208) -> 16#e4;
decode(209) -> 16#eb;
decode(210) -> 16#e6;
decode(211) -> 16#85;
decode(212) -> 16#14;
decode(213) -> 16#48;
decode(214) -> 16#dd;
decode(215) -> 16#38;
decode(216) -> 16#2a;
decode(217) -> 16#cc;
decode(218) -> 16#7f;
decode(219) -> 16#b1;
decode(220) -> 16#c0;
decode(221) -> 16#71;
decode(222) -> 16#96;
decode(223) -> 16#f8;
decode(224) -> 16#3f;
decode(225) -> 16#28;
decode(226) -> 16#f2;
decode(227) -> 16#69;
decode(228) -> 16#74;
decode(229) -> 16#68;
decode(230) -> 16#b7;
decode(231) -> 16#a3;
decode(232) -> 16#50;
decode(233) -> 16#d0;
decode(234) -> 16#79;
decode(235) -> 16#1d;
decode(236) -> 16#fc;
decode(237) -> 16#ce;
decode(238) -> 16#8a;
decode(239) -> 16#8d;
decode(240) -> 16#2e;
decode(241) -> 16#62;
decode(242) -> 16#30;
decode(243) -> 16#ea;
decode(244) -> 16#ed;
decode(245) -> 16#2b;
decode(246) -> 16#26;
decode(247) -> 16#b9;
decode(248) -> 16#81;
decode(249) -> 16#7c;
decode(250) -> 16#46;
decode(251) -> 16#89;
decode(252) -> 16#73;
decode(253) -> 16#a2;
decode(254) -> 16#f7;
decode(255) -> 16#72.

%%
%% Local Functions
%%
