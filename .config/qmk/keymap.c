#include QMK_KEYBOARD_H
#include <stdio.h>

// Left-hand home row mods
#define GUI_A LGUI_T(KC_A)
#define ALT_R LALT_T(KC_R)
#define CTL_S LCTL_T(KC_S)
#define SHFT_T LSFT_T(KC_T)

// Right-hand home row mods
#define SFT_N RSFT_T(KC_N)
#define CTL_E RCTL_T(KC_E)
#define ALT_I LALT_T(KC_I)
#define GUI_O RGUI_T(KC_O)

//Ru row mods

// Left-hand home row mods
#define GUI_X LGUI_T(KC_X)
#define ALT_P LALT_T(KC_P)
#define CTL_D LCTL_T(KC_D)
#define SHFT_Y LSFT_T(KC_Y)

// Right-hand home row mods
#define SFT_B RSFT_T(KC_B)
#define CTL_F RCTL_T(KC_F)
#define ALT_J LALT_T(KC_J)
#define RUGUI_T RGUI_T(KC_T)

enum custom_keycodes {
    M_WQ = SAFE_RANGE,
	M_PASS,
    M_GAPASS,
    M_2LBRC,
    M_CTV,
    M_RU,
    M_EN,
};

#define COMBO_COUNT 16
uint16_t COMBO_LEN = COMBO_COUNT;
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

//COLEMAK
    [0] = LAYOUT_split_3x6_3(
    //,-----------------------------------------------------.       ,--------------------------------------:/;------------.
         KC_TAB,    KC_Q,    KC_W,    KC_F,    KC_P,    KC_G,            KC_J,    KC_L,    KC_U,    KC_Y, KC_SCLN, KC_PAST,
    //|--------+--------+--------+--------+--------+--------|       |--------+--------+--------+--------+--------+--------|
        KC_BSPC,   GUI_A,   ALT_R,   CTL_S,  SHFT_T,    KC_D,             KC_H,  SFT_N,   CTL_E,   ALT_I,   GUI_O,  KC_EQL,
    //|--------+--------+--------+--------+--------+--------|       |--------+--------+--------+--------+--------+--------|
  KC_MS_WH_DOWN,    KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,            KC_K,    KC_M, KC_COMM,  KC_DOT, KC_SLSH,  KC_ESC,
    //|--------+--------+--------+--------+--------+--------|       |--------+--------+--------+--------+--------+--------|
                                    KC_F11,   MO(4),  KC_SPC,          KC_ENT,   MO(5), KC_CAPS
                                //`--------------------------'      `--------------------------'
  ),
//DEFAULT QWERTY
    [1] = LAYOUT_split_3x6_3(
  //,-----------------------------------------------------.                    ,-----------------------------------------------------.
       KC_TAB,    KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,                         KC_Y,    KC_U,    KC_I,    KC_O,   KC_P,  KC_ESC,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      KC_BSPC,    KC_A,    KC_S,    KC_D,    KC_F,    KC_G,                         KC_H,    KC_J,    KC_K,    KC_L, KC_SCLN, KC_QUOT,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      KC_LSFT,    KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                         KC_N,    KC_M, KC_COMM,  KC_DOT, KC_SLSH,  DF(0),
  //|--------+--------+--------+--------+--------+--------+--------|  |--------+--------+--------+--------+--------+--------+--------|
                                          _______,   MO(4),  KC_SPC,     KC_ENT,   MO(5), KC_F13
                                      //`--------------------------'  `--------------------------'
  ),
//RUSSIAN
 [2] = LAYOUT_split_3x6_3(
  //,-----------------------------------------------------.                    ,-------------------------------------------------#---.
       KC_O,    KC_W,    KC_R,    KC_K,   KC_COMM,   KC_Q,                       KC_RBRC,    KC_S,    KC_Z, KC_QUOT,    KC_A, S(KC_3),
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
    KC_BSPC, GUI_X,    ALT_P,    CTL_D,    SHFT_Y,   KC_N,                         KC_L,   SFT_B,   CTL_F,  ALT_J,   RUGUI_T,  KC_C,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      KC_SCLN, KC_LBRC,  KC_G,    KC_H,  KC_V, KC_U,                              KC_GRV,    KC_M,    KC_E,  KC_DOT,    KC_I, KC_SLSH,
  //|--------+--------+--------+--------+--------+--------+--------|  |--------+--------+--------+--------+--------+--------+--------|

                                          M_2LBRC, MO(4),  KC_SPC,      KC_ENT,   MO(5), M_EN
                                      //`--------------------------'  `--------------------------'
  ),
 //Gaming
 [3] = LAYOUT_split_3x6_3(
  //,-----------------------------------------------------.                    ,-----------------------------------------------------.
       KC_TAB,    KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,                         KC_1,    KC_2,    KC_3,    KC_4,   KC_5,  XXXXXXX,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      KC_LCTL,    KC_A,    KC_S,    KC_D,    KC_F,    KC_G,                         KC_6,    KC_7,    KC_8,  KC_9,    KC_0,   XXXXXXX,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      KC_LSFT,    KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                         KC_N,    KC_M, KC_COMM,  KC_DOT, KC_SLSH,  KC_ESC,
  //|--------+--------+--------+--------+--------+--------+--------|  |--------+--------+--------+--------+--------+--------+--------|
                                         XXXXXXX, KC_LSFT,  KC_SPC,      KC_E,   KC_ENT, DF(0)
                                      //`--------------------------'  `--------------------------'
  ),
//Nums arrows
 [4] = LAYOUT_split_3x6_3(
  //,-----------------------------------------------------.                    ,---"------'-----------&---------*--------$-----%-----.
      KC_MPLY,  KC_0 ,   KC_1,   KC_2,    KC_3,  KC_PMNS,                        KC_DQUO, KC_QUOT , KC_AMPR, KC_PAST, KC_DLR, KC_PERC,
  //|--------+---\----+--------+---------+--------+--------|                    |--------+--------+--------+---------+----(---+---)----|
      KC_TILD, KC_BSLS,  KC_4,     KC_5,    KC_6, KC_PLUS,                      KC_LEFT, KC_DOWN,  KC_UP,  KC_RIGHT, KC_LPRN, KC_RPRN,
  //|--------+---``----+--------+--------+--------+--------|                   |-----<--+--->-----+----{---+--}-----+----[---+---]----|
      KC_LSFT, KC_GRV ,  KC_7,    KC_8,     KC_9, KC_EQL ,                        KC_LT,   KC_GT,   KC_LCBR, KC_RCBR, KC_LBRC, KC_RBRC,
  //|--------+--------+--------+--------+--------+--------+--------|  |--------+--------+----|----+--------+--------+--------+--------|
                                          KC_LGUI, _______,  KC_SPC,     KC_ENT,   MO(6), KC_PIPE
                                      //`--------------------------'  `--------------------------'
  ),
//Special characters f1-12
  [5] = LAYOUT_split_3x6_3(
  //,------------!----------@------#---------$---------%--.                    ,---------------------------------------)--------------.
        M_CTV, KC_EXLM,   KC_AT, KC_HASH,  KC_DLR, KC_PERC,                      KC_WBAK,KC_WFWD, KC_WREF,  KC_F12, KC_RPRN, KC_SLEP,
  //|--------+--------+--------+--------+--------+--------|                    |--- - --+---[---+-----{--+--------+--------+--------|
       KC_F1,    KC_F2,   KC_F3,  KC_F4,   KC_F5,  KC_F11,                       KC_MINS, KC_LBRC,KC_LCBR, KC_RBRC, KC_BSLS,  TG(5),
  //|--------+--------+--------+--------+--------+--------|                    |----_---+--------+-------+--------+--------+--------|
      KC_F6,    KC_F7,   KC_F8,  KC_F9,    KC_F10, KC_F12,                       KC_UNDS, KC_PLUS, KC_LCBR, KC_RCBR, KC_PSLS,   TG(4),
  //|--------+--------+--------+--------+--------+--------+--------|  |--------+--------+--------+--------+--------+--------+--------|
                                          KC_LGUI,   MO(6),  KC_SPC,     KC_ENT, _______, KC_RALT
                                      //`--------------------------'  `--------------------------'
  ),
//Service Layer
  [6] = LAYOUT_split_3x6_3(
  //,-----------------------------------------------------.                    ,-----------------------------------------------------.
       QK_MAKE, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, KC_WSCH,                      KC_WBAK, KC_WFWD,KC_WREF, M_PASS,  M_GAPASS, KC_SLEP,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      RGB_TOG, RGB_HUI, XXXXXXX, KC_VOLU,  KC_BRIU, XXXXXXX,                     XXXXXXX, KC_STOP, KC_MPLY, XXXXXXX,   DF(1),  M_RU,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      RGB_MOD, RGB_HUD,  XXXXXXX, KC_VOLD, KC_BRID, XXXXXXX,                      XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, DF(3),
  //|--------+--------+--------+--------+--------+--------+--------|  |--------+--------+--------+--------+--------+--------+--------|
                                          KC_LGUI, _______,  KC_SPC,     KC_ENT, _______, KC_RALT
                                      //`--------------------------'  `--------------------------'
  )
};

void switch_language(void) {
  register_code(KC_LALT);
  register_code(KC_SPACE);
  unregister_code(KC_SPACE);
  unregister_code(KC_LALT);
}


bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    switch (keycode) {

    case M_CTV:
        if (record->event.pressed) {
            SEND_STRING(SS_LCTL("ct"));
            register_code(KC_I);
            unregister_code(KC_I);
            SEND_STRING(SS_LCTL("v"));
            register_code(KC_ENT);
            unregister_code(KC_ENT);
        }
        break;
    case M_2LBRC:
        if (record->event.pressed) {
            SEND_STRING("[[");
        }
        break;
    case M_WQ:
        if (record->event.pressed)
            SEND_STRING(":wq");
        else {
            register_code(KC_ENT);
            unregister_code(KC_ENT);
        }
        break;
    case M_PASS:
        if (record->event.pressed)
            SEND_STRING("Mblw16735");
        else {
            register_code(KC_ENT);
            unregister_code(KC_ENT);
        }
        break;
    case M_GAPASS:
        if (record->event.pressed)
            SEND_STRING("Ga7110732");
        else {
            register_code(KC_ENT);
            unregister_code(KC_ENT);
        }
        break;
    case M_RU:
        if (record->event.pressed)
        {
            layer_move(2);
            switch_language();
        }
        break;

    case M_EN:
        if (record->event.pressed)
        {
            layer_move(0);
            switch_language();
            register_code(KC_CAPS);
            unregister_code(KC_CAPS);
        }
        break;
   }
    return true;
};

const uint16_t PROGMEM LBRC[] = {KC_W, KC_F, COMBO_END};
const uint16_t PROGMEM LPRN[] = {KC_F, KC_P, COMBO_END};
const uint16_t PROGMEM LCBR[] = {KC_X, KC_C, COMBO_END};

const uint16_t PROGMEM RBRC[] = {KC_U, KC_Y, COMBO_END};
const uint16_t PROGMEM RPRN[] = {KC_L, KC_U, COMBO_END};
const uint16_t PROGMEM RCBR[] = {KC_COMM, KC_DOT, COMBO_END};

const uint16_t PROGMEM WQ[] = {KC_Y, KC_SCLN, COMBO_END};


const uint16_t PROGMEM GOTO1[] = {KC_TAB, KC_Q, COMBO_END};
const uint16_t PROGMEM GOTO2[] = {KC_BSPC, GUI_A, COMBO_END};
const uint16_t PROGMEM GOTO3[] = {KC_MS_WH_DOWN, KC_Z, COMBO_END};
const uint16_t PROGMEM GOTO4[] = {KC_Q, KC_W, COMBO_END};
const uint16_t PROGMEM GOTO5[] = {GUI_A, ALT_R, COMBO_END};
const uint16_t PROGMEM GOTO6[] = {KC_Z, KC_X, COMBO_END};
const uint16_t PROGMEM GOTO7[] = {KC_SCLN, KC_PAST, COMBO_END};
const uint16_t PROGMEM GOTO8[] = {GUI_O, KC_EQL, COMBO_END};
const uint16_t PROGMEM GOTO9[] = {KC_SLSH, KC_ESC, COMBO_END};

combo_t key_combos[COMBO_COUNT] = {
    COMBO(LPRN, KC_LPRN),
    COMBO(LBRC, KC_LBRC),
    COMBO(LCBR, KC_LCBR),

    COMBO(RPRN, KC_RPRN),
    COMBO(RBRC, KC_RBRC),
    COMBO(RCBR, KC_RCBR),
    COMBO(WQ, M_WQ),

    COMBO(GOTO1, LGUI(KC_W)),
    COMBO(GOTO2, LGUI(KC_F)),
    COMBO(GOTO3, LGUI(KC_P)),
    COMBO(GOTO4, LGUI(KC_R)),
    COMBO(GOTO5, LGUI(KC_S)),
    COMBO(GOTO6, LGUI(KC_T)),
    COMBO(GOTO7, LGUI(KC_7)),
    COMBO(GOTO8, LGUI(KC_8)),
    COMBO(GOTO9, LGUI(KC_9)),
};
