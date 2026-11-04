module LordOfTheRings.Ally.Cards.RosieCotton (rosieCotton) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Trait (Trait (Hobbit))

newtype RosieCotton = RosieCotton AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rosieCotton :: AllyCard RosieCotton
rosieCotton = ally RosieCotton Cards.rosieCotton

-- | Action: Exhaust Rosie Cotton to choose a Hobbit hero and an attribute (willpower,
-- attack, defense). Add Rosie Cotton's willpower to that hero's chosen attribute until the
-- end of the phase. (Limit once per phase.)
instance HasAbilities RosieCotton where
  getAbilities (RosieCotton a) =
    [ limited (PlayerLimit PerPhase 1)
        $ restricted a 1 (exists $ HeroWithTrait Hobbit)
        $ ActionAbility (exhaustCost a)
    ]

instance RunMessage RosieCotton where
  runMessage msg s@(RosieCotton a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      -- her willpower as the effect resolves
      x <- getWillpower (toTarget a)
      hobbits <- select (HeroWithTrait Hobbit)
      chooseOrRunOneM pid do
        questionLabeled "rosieCotton.chooseHero"
        causedBy a
        targets hobbits \hid -> chooseOneM pid do
          questionLabeled "rosieCotton.chooseAttribute"
          causedBy a
          labeled "rosieCotton.willpower" $ phaseModifier a hid (WillpowerModifier x)
          labeled "rosieCotton.attack" $ phaseModifier a hid (AttackModifier x)
          labeled "rosieCotton.defense" $ phaseModifier a hid (DefenseModifier x)
      pure s
    _ -> RosieCotton <$> runMessage msg a
