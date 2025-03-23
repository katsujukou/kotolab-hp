export default {
  content: ["./index.html", "./src/**/*.purs", "./src/**/*.js"],
  theme: {
    extend: {},
    fontFamily: {
      "genei": [
        '"GenEi Roman Notes"',
      ],
      "yomogi": [
        "Yomogi"
      ],
      "retro": ['"Kaisei Decol"', "Yomogi"],
      "nanakyun": ["Nanakyun", "Yomogi"],
      "miama": ["Miama"],
      "allura": ["Allura"],
      "josefin-sans": ['"Josefin Sans"'],
      "codeblock": ["'HackGen Console NF'"]
    },
    plugins: [],
  },
  safelist: [
    "translate-x-full",
    "translate-x-0",
    "opacity-100",
    "opacity-0",
    "rotate-0",
    "rotate-180"
  ]
}
