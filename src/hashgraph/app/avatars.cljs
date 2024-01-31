(ns hashgraph.app.avatars
  (:require [hashgraph.utils :refer-macros [l]]))

(def avatar-size 40)

(def male-haircuts
  [[:path
    {:class "avatar-haircut",
     :d
     "M183.49,86.48v0a22.09,22.09,0,0,1-3.19,3c-.09.09-.51.4-1.06.82-.91.65-1.84,1.29-2.8,1.91a67.62,67.62,0,0,1-70.47,2,70.66,70.66,0,0,1-6.09-4c-.67-.49-1.31-1-1.95-1.48a64.57,64.57,0,0,1-5-4.28c-.73-.71-1.46-1.44-2.17-2.2-.93-1-1.84-2-2.7-3a4.25,4.25,0,0,1-.82-1.44,64.5,64.5,0,0,0,5.69,5v0a67.72,67.72,0,0,0,8,5.43A67.63,67.63,0,0,0,163.23,91a68,68,0,0,0,11-6.51l.4-.29.44-.33a1.88,1.88,0,0,0,.25-.2A24.35,24.35,0,0,0,180,78.85a4.49,4.49,0,0,0,.84-3.55A35.43,35.43,0,0,1,183.49,86.48Z",
     :id    "HairsShadow"}
    [:path
     {:class     "avatar-haircut",
      :d
      "M180,78.85a24.35,24.35,0,0,1-4.63,4.77,1.88,1.88,0,0,1-.25.2l-.44.33-.4.29a68,68,0,0,1-11,6.51A67.63,67.63,0,0,1,101,88.29a67.72,67.72,0,0,1-8-5.43v0a64.5,64.5,0,0,1-5.69-5c-.49-.49-1-1-1.44-1.46-.94-1-1.82-2-2.69-3a4.58,4.58,0,0,1,0-5.89A67.45,67.45,0,0,1,95.62,56q1.89-1.36,3.92-2.59a67.07,67.07,0,0,1,35.14-9.87c3.17,0,11.93,0,19.91,3,8.55,3.2,14.74,9.27,19,15.21a63,63,0,0,1,7,12.57,3.53,3.53,0,0,1,.26,1A4.49,4.49,0,0,1,180,78.85Z",
      :data-name "Hairs",
      :id        "Hairs-3"}]]])

(defn male-avatar [color background-color]
  (let [wallpaper-color background-color #_"white"
        sweater-color color
        hair-color
        ;;color
        "#2d2d2d"
        ]
    [:svg
     {:width     avatar-size,
      :height    avatar-size,
      :viewBox   "0 0 256 256",
      :data-name "Layer 1",
      :id        "Layer_1",
      :xmlns     "http://www.w3.org/2000/svg"}
     [:defs
      [:style
       ".cls-3{fill:#7c211a;}.cls-4{fill:#eab198;}.cls-6{opacity:0.2;}.cls-7{opacity:0.3;}"]]
     [:g
      {:data-name "Male 3", :id "Male_3"}
      [:path
       {:fill wallpaper-color
        :d
        "M250.43,128a121.66,121.66,0,0,1-37.86,88.34H44.85a102.15,102.15,0,0,1-9-8.94l-.83-1C28,198.08,5.26,168.49,6.44,128,8.07,71.91,55.22,6,128.44,6A122,122,0,0,1,250.43,128Z",
        :id    "Wallpaper"
        :style {:transition "fill 0.4s ease-in"}}]
      [:path
       {:fill sweater-color
        :d
        "M221.87,206.47a129.62,129.62,0,0,1-9.3,9.88A121.64,121.64,0,0,1,128.44,250c-39.67,0-69.4-21.07-83.58-33.65a108,108,0,0,1-9.06-8.95c-.22-.29-.51-.6-.82-1C48.61,192.7,66.92,182.05,88,176c5.91,14.7,21.76,25.22,40.4,25.22,17.32,0,32.22-9.08,39-22.13.53-1,1-2,1.42-3.09l1.15.33C190.59,182.45,208.51,193,221.87,206.47Z",
        :id    "Sweater"}]
      [:path
       {:class "cls-3",
        :d
        "M168.82,176c-.42,1-.89,2.06-1.42,3.09-6.77,13.05-21.67,22.13-39,22.13-18.64,0-34.49-10.52-40.4-25.22.45-.13.89-.24,1.33-.38a4.81,4.81,0,0,0,.65-.18l.69-.17a1.42,1.42,0,0,1,.22-.07s1.75-.47,3.42-.84a37.9,37.9,0,0,0,68.19,0A28.44,28.44,0,0,1,168.82,176Z",
        :id    "Neckband"}]
      [:path
       {:class "cls-4",
        :d
        "M162.51,174.37A38.09,38.09,0,0,1,145,191.9a37.9,37.9,0,0,1-50.68-17.53c1.35-.34,2.7-.65,4.08-.91.33-.07.67-.14,1-.23l.51-.08.87-.18,1.2-.2c.29-.07.6-.11.91-.18H103c.33-.86.68-1.8,1-2.82.12-.33.23-.71.34-1.06a33.22,33.22,0,0,0,.86-4.07,41.84,41.84,0,0,0,6.93,4.6,5.45,5.45,0,0,1,.53.29c.2.09.42.2.62.31l.76.35.44.2c.11.07.25.11.38.18s.31.13.44.2.32.11.45.18h0a1.76,1.76,0,0,1,.2.09l.53.2c.2.09.43.17.67.26h0l.74.27.15,0c.16.07.33.11.51.18s.51.16.76.22.24.07.37.09l.69.2a38.73,38.73,0,0,0,4.55.8c.16,0,.31,0,.47,0l.33,0,.62,0,.56,0a4.29,4.29,0,0,1,.62,0h.56a30.69,30.69,0,0,0,9.74-1.62.07.07,0,0,0,.07,0,1.57,1.57,0,0,0,.35-.11c.8-.26,1.49-.55,2.13-.8l.85-.35a1.65,1.65,0,0,1,.22-.09,38.51,38.51,0,0,0,9.88-6.13c.31,1.38.69,2.84,1.15,4.38.42,1.37.87,2.66,1.31,3.88.38.07.74.11,1.11.2s.74.13,1.09.2l1.09.2c.26,0,.53.11.8.16s.35.06.53.11C159.83,173.75,161.18,174,162.51,174.37Z",
        :id    "Neck"}]
      [:path
       {:fill hair-color
        :d
        "M197.68,81.66a7.51,7.51,0,0,1-.36,2.42c-1.6,5.64-7.1,6.22-8.12,11.15-.2.91.08.44,0,3.24-.07,2-.23,6.59-1.54,9.59a10.57,10.57,0,0,1-3.28,4,6.64,6.64,0,0,0-4.15-2.95,6,6,0,0,0-2.38,0,66.89,66.89,0,0,0-.93-14.77,66,66,0,0,0-2.58-9.85c-.46-1.4-1-2.8-1.55-4.18a74.58,74.58,0,0,0-8.63-15.49c-8.64-11.79-21-20.56-35.9-20.51a39.75,39.75,0,0,0-24.46,8.88c-7.64,6-13.77,14.32-18,23.22A72,72,0,0,0,79.91,94.4a67,67,0,0,0-1,14.77,5.47,5.47,0,0,0-2.35,0,7,7,0,0,0-4.62,3.7,10.12,10.12,0,0,1-3.33-3.7c-2.29-4.82,1.11-7.51-1.27-12.48-2.11-4.42-6.1-4.84-7.23-10a12.36,12.36,0,0,1-.18-3.79c.53-4.73,3.4-5,4.35-8.77,1.33-5.31-3.51-8.39-1.35-14.92a5.8,5.8,0,0,1,.55-1.38c2.75-4.79,10.3-1.24,14.08-5.12,3.48-3.58-.25-9.37,4.72-14.26a7.79,7.79,0,0,1,2.11-1.59c5.6-2.71,10.9,4.92,15.85,3,4-1.54,2.56-7.09,8.19-10.33a12.24,12.24,0,0,1,6.64-1.64c6.4.35,8,6.1,12.12,5.55,4-.56,3.8-6,9.22-7.68a11.24,11.24,0,0,1,9,1.06c5,3.09,3.53,8.64,7.5,10.13,3.2,1.22,4.95-2.16,11.13-1.8a13.25,13.25,0,0,1,7.23,2c5.58,3.93,2.09,11.1,7.13,14.72,3.53,2.55,7.13.37,12.21,4.35a8.76,8.76,0,0,1,2.71,2.79c2.53,4.51-2.47,8-.82,13.19C193.81,76.29,197.68,77.16,197.68,81.66Z",
        :id    "Hairs"}]
      [:path
       {:class "cls-6",
        :d
        "M170,176.34c-.84,1-1.69,1.87-2.57,2.76a58.26,58.26,0,0,1-6.29,5.46,43.63,43.63,0,0,1-9.5,5.39,35.15,35.15,0,0,1-6.61,2,28.63,28.63,0,0,1-6.06.51A31.79,31.79,0,0,1,127.33,190c-.23-.09-.45-.18-.67-.29a41.62,41.62,0,0,1-8.3-4.73,59.52,59.52,0,0,1-12.68-12.85c-.58-.78-1.13-1.58-1.69-2.38.12-.33.23-.71.34-1.06a33.22,33.22,0,0,0,.86-4.07c.6.47,1.2.94,1.82,1.38a44.9,44.9,0,0,0,5.11,3.22,5.45,5.45,0,0,1,.53.29c.2.09.42.2.62.31l.76.35.44.2a4.11,4.11,0,0,0,.38.18c.16.07.31.13.44.2s.32.11.45.18h0a1.76,1.76,0,0,1,.2.09l.53.2.67.26h0l.74.27.15,0c.16.07.33.11.51.18s.51.16.76.22.24.07.37.09l.69.2a31,31,0,0,0,4.55.8c.16,0,.31,0,.47,0l.33,0,.62,0,.56,0a4.29,4.29,0,0,1,.62,0h.56a32.12,32.12,0,0,0,9.74-1.62.07.07,0,0,0,.07,0,1.57,1.57,0,0,0,.35-.11c.67-.22,1.31-.46,2-.73a.67.67,0,0,0,.17-.07l.85-.35a1.65,1.65,0,0,1,.22-.09,45.91,45.91,0,0,0,8.26-4.88c.55-.4,1.09-.83,1.62-1.25.31,1.38.69,2.84,1.15,4.38.42,1.37.87,2.66,1.31,3.88.38.07.74.11,1.11.2s.74.13,1.09.2l1.09.2c.26,0,.53.11.8.16s.35.06.53.11c1.38.29,2.73.57,4.06.91,2.13.49,4.24,1,6.31,1.64Z",
        :id    "NeckShadow"}]
      [:path
       {:class "cls-4",
        :d
        "M186.54,120c-.16,5.2-3.48,10.46-7.19,12.39a7.27,7.27,0,0,1-1.57.61l-.12,0a4.9,4.9,0,0,1-2,.06,5,5,0,0,1-2-.88,2.83,2.83,0,0,1-.28-.2,7.34,7.34,0,0,1-1.12-1.11c-2.68-3.23-3.78-9.29-2-14.11a14.48,14.48,0,0,1,3.24-5.06,9,9,0,0,1,4.42-2.6,5.91,5.91,0,0,1,2.37,0C184.64,110,186.67,115.7,186.54,120Z",
        :id    "RightEar"}]
      [:path
       {:class "cls-7",
        :d
        "M186.53,120c-.15,5.2-3.46,10.46-7.19,12.39a7.07,7.07,0,0,1-1.55.6.34.34,0,0,1-.14,0,4.55,4.55,0,0,1-2,.07,4.92,4.92,0,0,1-2-.87c-.09-.06-.18-.15-.29-.22a6.48,6.48,0,0,1-1.11-1.11c-2.51-3-3.62-8.5-2.29-13.14.09-.33.2-.64.31-1a14.75,14.75,0,0,1,3.24-5.07,9,9,0,0,1,4.42-2.59,6,6,0,0,1,2.38,0,6.64,6.64,0,0,1,4.15,2.95A13.9,13.9,0,0,1,186.53,120Z",
        :id    "RightEarShadow"}]
      [:path
       {:class "cls-4",
        :d
        "M84.61,130.92a6.5,6.5,0,0,1-1.24,1.22c0,.05-.11.07-.15.11a5.07,5.07,0,0,1-2,.87,5,5,0,0,1-2.06-.07.26.26,0,0,1-.11,0,7.5,7.5,0,0,1-1.58-.6c-3.7-1.93-7-7.19-7.19-12.39a14.23,14.23,0,0,1,1.71-7.19,7,7,0,0,1,4.62-3.7,5.47,5.47,0,0,1,2.35,0,8.66,8.66,0,0,1,4.44,2.59,14.73,14.73,0,0,1,3.24,5,11.07,11.07,0,0,1,.44,1.49C88.14,122.82,87,128,84.61,130.92Z",
        :id    "LeftEar"}]
      [:path
       {:class "cls-4",
        :d
        "M177.85,109.17q-.14,3.28-.6,6.59a78.11,78.11,0,0,1-4,16.27c-4.66,13.1-12.48,24.71-21.91,32.3-.53.42-1.07.85-1.62,1.25a45.91,45.91,0,0,1-8.26,4.88,1.65,1.65,0,0,0-.22.09l-.85.35a.67.67,0,0,1-.17.07c-.65.27-1.29.51-2,.73a1.57,1.57,0,0,1-.35.11.07.07,0,0,1-.07,0,32.12,32.12,0,0,1-9.74,1.62h-.56a4.29,4.29,0,0,0-.62,0l-.56,0-.62,0-.33,0c-.16,0-.31,0-.47,0a31,31,0,0,1-4.55-.8l-.69-.2c-.13,0-.24-.06-.37-.09s-.51-.15-.76-.22-.35-.11-.51-.18l-.15,0-.74-.27h0l-.67-.26L116,171a1.76,1.76,0,0,0-.2-.09h0c-.13-.07-.29-.11-.45-.18s-.28-.13-.44-.2a4.11,4.11,0,0,1-.38-.18l-.44-.2-.76-.35c-.2-.11-.42-.22-.62-.31a5.45,5.45,0,0,0-.53-.29A44.9,44.9,0,0,1,107,166c-.62-.44-1.22-.91-1.82-1.38a58.92,58.92,0,0,1-8.57-8.52,77.06,77.06,0,0,1-13.25-24c-.15-.35-.26-.73-.4-1.11a78,78,0,0,1-3.42-14.65c-.17-1.2-.31-2.4-.42-3.62-.09-1-.15-2-.2-3.08,0-.18,0-.34,0-.51a67,67,0,0,1,1-14.77,72,72,0,0,1,5.88-17.93c4.24-8.9,10.37-17.27,18-23.22a39.75,39.75,0,0,1,24.46-8.88c14.92,0,27.26,8.72,35.9,20.51a74.58,74.58,0,0,1,8.63,15.49c.56,1.38,1.09,2.78,1.55,4.18a66,66,0,0,1,2.58,9.85A66.89,66.89,0,0,1,177.85,109.17Z",
        :id    "Head"}]
      [:path
       {:class "cls-6",
        :d
        "M177.7,100.29A30.21,30.21,0,0,1,170,98.84a3.7,3.7,0,0,1-.4-.15c-7.94-2.73-7.72-7.62-14.25-9.3-7.37-1.89-10.12,3.7-18.2,1.6-6.15-1.6-6-5.2-10.95-5.58-7.39-.55-10.32,7.77-16.53,6.51-4-.82-4.42-4.66-9.33-5.75a13.73,13.73,0,0,0-9.21,1.71h-.05c2.38-2.29,3-5,6.4-5.86,4.66-1.18,6.19,3.59,12.19,3.53,7.77-.09,9.59-7,16.53-6.51,5,.38,4.8,4,10.95,5.57,8.08,2.11,10.83-3.5,18.2-1.59,6.53,1.68,6.31,6.57,14.25,9.3a28.91,28.91,0,0,0,7.24,1.55s.13,1.07.42,3.15C177.65,100.09,177.7,100.29,177.7,100.29Z",
        :id    "HairsShadow"}]
      [:path
       {:fill      hair-color
        :d         "M183.16,92.32a6.15,6.15,0,0,1-2.95,1.37l-.36.07a13.14,13.14,0,0,1-3,.11,28.91,28.91,0,0,1-7.24-1.55c-7.94-2.73-7.72-7.62-14.25-9.3-7.37-1.91-10.12,3.7-18.2,1.59-6.15-1.59-6-5.19-10.95-5.57-6.94-.53-8.76,6.42-16.53,6.51-6,.06-7.53-4.71-12.19-3.53-3.38.84-4,3.57-6.4,5.86a10.26,10.26,0,0,1-2.84,2,15.23,15.23,0,0,1-7.63,1.24c-2.82-.27-5.42-1.33-6.73-3.13-3.71-5.11,3.09-15.36,6-19.76,15.21-23,41.65-27.77,45.09-28.32.89-.16,2.06-.34,3.66-.52,8.08-.86,25.6-1.19,39.74,11C183.49,63.46,189.31,86.81,183.16,92.32Z",
        :data-name "Hairs",
        :id        "Hairs-2"}]]]))

(defn female-avatar [color background-color]
  (let [wallpaper-color background-color #_"white" ;; #84d0f7
        sweater-color color]
    [:svg
     {:width     avatar-size,
      :height    avatar-size,
      :viewBox   "0 0 256 256",
      :data-name "Layer 1",
      :id        "Layer_1",
      :xmlns     "http://www.w3.org/2000/svg"}
     [:defs
      [:style
       ".female-cls-2{fill:#2d2d2d;}.female-cls-3{fill:#050505;}.female-cls-3,.female-cls-7{opacity:0.2;}.female-cls-5{fill:#7c211a;}.female-cls-6{fill:#e59973;}.female-cls-8{opacity:0.3;}}"]]
     [:g
      {:data-name "Female 1", :id "Female_1"}
      [:path
       {:id   "Wallpaper"
        :fill wallpaper-color
        :d    "M249.16,127.72a120.25,120.25,0,0,1-28.27,77.63,123.4,123.4,0,0,1-9.2,9.79h-166a101.12,101.12,0,0,1-8.95-8.86c-.24-.27-.51-.59-.81-.95C29,197.07,6.56,167.78,7.72,127.72,9.33,72.21,56,7,128.44,7A120.72,120.72,0,0,1,249.16,127.72Z"
        :style {:transition "fill 0.4s ease-in"}}]
      [:circle
       {:class "female-cls-2",
        :cx    "128.23",
        :cy    "41.64",
        :id    "Chignon",
        :r     "22.11"}]
      [:circle
       {:class "female-cls-3",
        :cx    "128.23",
        :cy    "41.64",
        :id    "ChignonShadow",
        :r     "22.11"}]
      [:path
       {:id    "Sweater"
        :fill  sweater-color
        :d    "M220.88,205.35A119.78,119.78,0,0,1,191,231a119.83,119.83,0,0,1-62.56,17.47c-25.7,0-47.17-8.93-62.88-18.6a142.57,142.57,0,0,1-19.84-14.68,99,99,0,0,1-8.93-8.87c-.24-.26-.52-.59-.83-1A111.23,111.23,0,0,1,61,186.53a130.17,130.17,0,0,1,27.42-11.3q19.91,17.6,39.78,35.18,20.11-17.59,40.18-35.18c.28.09.57.15.85.24a1.84,1.84,0,0,0,.28.09,132,132,0,0,1,26.25,10.93A112.71,112.71,0,0,1,220.88,205.35Z",}]
      [:path
       {:class "female-cls-5",
        :d
        "M168.4,175.23q-20.07,17.6-40.18,35.18-19.87-17.59-39.78-35.18a27,27,0,0,1,6.23-1.63Q111.43,188,128.2,202.37q17-14.42,34-28.77A28.66,28.66,0,0,1,168.4,175.23Z",
        :id    "Neckband"}]
      [:path
       {:class "female-cls-6",
        :d
        "M162.17,173.6q-17,14.37-34,28.77Q111.45,188,94.67,173.6c1.44-.35,2.94-.67,4.46-1s2.78-.54,4.13-.76a37.39,37.39,0,0,0,1.65-5.13c.22-1,.39-1.87.54-2.74a40.76,40.76,0,0,0,4.3,3.07,46.13,46.13,0,0,0,4.44,2.43,36.36,36.36,0,0,0,13.4,3.22,36.86,36.86,0,0,0,14.91-3.5,53,53,0,0,0,4.89-2.78c1.52-1,2.78-2,3.74-2.74.28,1.28.63,2.63,1.06,4s.89,2.85,1.39,4.13c1.18.2,2.37.41,3.61.67C158.91,172.84,160.56,173.21,162.17,173.6Z",
        :id    "Neck"}]
      [:path
       {:class "female-cls-7",
        :d
        "M169.53,175.56c-.82.93-1.67,1.84-2.54,2.71a55,55,0,0,1-6.22,5.41,42.25,42.25,0,0,1-9.4,5.33,34.85,34.85,0,0,1-6.54,1.93,29.43,29.43,0,0,1-6,.52,31.6,31.6,0,0,1-11.5-2.39l-.65-.26a42.05,42.05,0,0,1-8.21-4.69,59.67,59.67,0,0,1-12.56-12.71c-.57-.77-1.11-1.55-1.65-2.35.11-.35.21-.7.3-1.07a34.18,34.18,0,0,0,.89-4c.59.48,1.18.94,1.78,1.37a42.73,42.73,0,0,0,5.07,3.18c.17.08.34.19.52.28s.41.21.63.32l.74.35.43.2c.13.06.26.13.37.17l.46.2.43.17a1,1,0,0,0,.22.09c.17.08.35.15.52.21s.44.16.67.24c0,0,0,0,0,0,.26.09.47.18.71.24l.16.07c.17,0,.34.11.52.15a6.12,6.12,0,0,0,.76.24,2,2,0,0,1,.35.09l.69.19a29.44,29.44,0,0,0,4.5.78,2.36,2.36,0,0,0,.48.05,1.51,1.51,0,0,0,.3,0c.22,0,.41,0,.63,0s.35,0,.54.05h1.18a31,31,0,0,0,9.62-1.59l.07,0,.37-.13c.65-.2,1.28-.46,1.93-.7l.18-.08.82-.35c.07,0,.13-.07.22-.09a44,44,0,0,0,8.17-4.82l1.61-1.24c.3,1.35.67,2.8,1.13,4.32.41,1.37.87,2.65,1.32,3.85l1.09.2,1.09.19,1.06.2c.26.06.52.1.8.17a5,5,0,0,1,.53.11c1.34.28,2.69.56,4,.89,2.1.5,4.17,1,6.23,1.63Z",
        :id    "NeckShadow"}]
      [:path
       {:class "female-cls-6",
        :d
        "M185.93,119.84c-.15,5.14-3.44,10.35-7.11,12.27a7.65,7.65,0,0,1-1.56.59l-.12,0a4.93,4.93,0,0,1-2,.06,5,5,0,0,1-2-.87,2.92,2.92,0,0,1-.28-.21,6.78,6.78,0,0,1-1.11-1.1c-2.65-3.19-3.73-9.18-2-14a14.46,14.46,0,0,1,3.2-5,8.77,8.77,0,0,1,4.38-2.57,5.76,5.76,0,0,1,2.34,0C184.05,109.94,186.06,115.55,185.93,119.84Z",
        :id    "RightEar"}]
      [:path
       {:class "female-cls-8",
        :d
        "M185.94,119.84c-.18,5.15-3.46,10.34-7.13,12.28a8.27,8.27,0,0,1-1.54.59l-.13,0a5.29,5.29,0,0,1-2,.06,5.2,5.2,0,0,1-2-.87l-.28-.21a8.13,8.13,0,0,1-1.11-1.09c-2.65-3.19-3.73-9.19-1.95-14a14.48,14.48,0,0,1,3.19-5,10.12,10.12,0,0,1,3.11-2.17,5.9,5.9,0,0,1,1.28-.39,5.63,5.63,0,0,1,2.35,0c.11,0,.24,0,.35.09C184.16,110.24,186.07,115.67,185.94,119.84Z",
        :id    "RightEarShadow"}]
      [:path
       {:class "female-cls-6",
        :d
        "M85.08,130.62a7.36,7.36,0,0,1-1.39,1.3,4.9,4.9,0,0,1-4,.81l-.12,0a7.15,7.15,0,0,1-1.56-.6c-3.67-1.91-6.95-7.12-7.11-12.26-.13-4.29,1.88-9.9,6.25-10.77,2.48-.5,4.86.7,6.73,2.58a14.51,14.51,0,0,1,3.2,5C88.81,121.43,87.72,127.42,85.08,130.62Z",
        :id    "LeftHear"}]
      [:path
       {:class "female-cls-6",
        :d
        "M177.33,109.09c0,.39,0,.8-.06,1.21a78.32,78.32,0,0,1-4.46,21.41c-4.6,13-12.36,24.46-21.68,32l-1.61,1.24a44,44,0,0,1-8.17,4.82c-.09,0-.15.07-.22.09l-.82.35-.18.08c-.65.24-1.28.5-1.93.7l-.37.13-.07,0a31,31,0,0,1-9.62,1.59H127c-.19,0-.37,0-.54-.05s-.41,0-.63,0a1.51,1.51,0,0,1-.3,0,2.36,2.36,0,0,1-.48-.05,29.44,29.44,0,0,1-4.5-.78l-.69-.19a2,2,0,0,0-.35-.09,6.12,6.12,0,0,1-.76-.24c-.18,0-.35-.11-.52-.15L118,171c-.24-.06-.45-.15-.71-.24,0,0,0,0,0,0-.23-.08-.45-.15-.67-.24s-.35-.13-.52-.21a1,1,0,0,1-.22-.09l-.43-.17-.46-.2c-.11,0-.24-.11-.37-.17l-.43-.2-.74-.35c-.22-.11-.44-.21-.63-.32s-.35-.2-.52-.28a42.73,42.73,0,0,1-5.07-3.18c-.6-.43-1.19-.89-1.78-1.37a46.22,46.22,0,0,1-4.39-3.93,53.28,53.28,0,0,1-4.1-4.48v0a77.71,77.71,0,0,1-13.52-24.81,80,80,0,0,1-3.78-18.08c-.09-1-.15-2-.2-3.06a66.81,66.81,0,0,1,.33-10.78c.15-1.46.37-2.89.63-4.33a54,54,0,0,1,1.22-5.28,72.94,72.94,0,0,1,4.6-12.47c4.2-8.8,10.26-17.08,17.82-23A39.21,39.21,0,0,1,128.27,45c14.75-.07,27,8.6,35.5,20.27a70.71,70.71,0,0,1,8.54,15.34c.57,1.37,1.09,2.74,1.57,4.13a69.76,69.76,0,0,1,2.54,9.75c.24,1.39.46,2.81.61,4.22A69.19,69.19,0,0,1,177.33,109.09Z",
        :id    "Head"}]
      [:path
       {:class "female-cls-7",
        :d
        "M161.93,112.26c-4.91-.13-12.69-4.28-17-7.22-12.69-8.6-22.53-25-29.64-25.31a5.76,5.76,0,0,0-2.23.33c-3.22,1.19-4.31,5.38-5.46,9.45-1.41,5-2.35,8.26-3,10.34-.07-5.71-.18-11.93,3.28-20.77,1-2.63,2.32-5.33,4.93-6.28,1.26-.48,5.39-1.48,15.88,8.21a62.41,62.41,0,0,1,7.57,8.17c8.54,11.15,8.62,12,13,15.86C155.43,110.56,159.21,110.28,161.93,112.26Z",
        :id    "HairsShadow"}]
      [:path
       {:class "female-cls-2",
        :d
        "M193.78,170.41c-.46,1.21-9.13.89-16.67-3.85-8.77-5.52-15.34-16.62-15.71-28.21-.36-12.19,6.35-17,2.57-23.7a7.77,7.77,0,0,0-2-2.39h0c-2.72-2-6.5-1.7-12.69-7.22-4.33-3.84-4.41-4.71-13-15.86A62.41,62.41,0,0,0,128.72,81c-10.49-9.69-14.62-8.69-15.88-8.21-2.61,1-3.89,3.65-4.93,6.28-3.46,8.84-3.35,15.06-3.28,20.77,0,4.87.06,9.37-2.18,14.8-4.76,11.51-13.25,11.84-13.73,22-.28,5.74,2.35,7,2.2,14.48,0,2.52-.24,11.86-6.11,18.16-9.8,10.47-29.51,6.19-29.77,4s18.21-2,21.79-11.54c2.92-7.78-8-13.65-6.41-26.93.79-6.77,3.87-7.32,6.41-15.68,3.74-12.39-1.26-17-.41-28.49,1.48-20,18.86-37,34.38-43.61a49.51,49.51,0,0,1,15.06-3.85c20.68-1.76,36.24,10.89,42,16.34,4.18,4,14.63,13.95,16.35,28.51a62.86,62.86,0,0,1-1,19.56c-2.76,15-7.63,16.71-8.65,28.83-.41,4.83-1.19,14.21,4.17,22.1C184.52,167.1,194.26,169.08,193.78,170.41Z",
        :id    "Hairs"}]]]))