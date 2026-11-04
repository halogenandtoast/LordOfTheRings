module LordOfTheRings.Hero.Cards.DainIronfootSpirit (dainIronfootSpirit) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype DainIronfootSpirit = DainIronfootSpirit HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dainIronfootSpirit :: HeroCard DainIronfootSpirit
dainIronfootSpirit = hero DainIronfootSpirit Cards.dainIronfootSpirit

-- | While Dáin Ironfoot is defending, he gains: "Action: Discard the top card of your deck
-- to give Dáin Ironfoot +1 [defense] for this attack. (Limit 3 times per phase.)"
instance HasAbilities DainIronfootSpirit where
  getAbilities (DainIronfootSpirit a) =
    [ limited (PlayerLimit PerPhase 3)
        $ restricted a 1 (exists (HeroWithId a.id <> DefendingHero) <> PlayerExists (PlayerWithId a.owner <> PlayerWithCardsInDeck 1))
        $ ActionAbility Free
    ]

instance RunMessage DainIronfootSpirit where
  runMessage msg h@(DainIronfootSpirit attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      discardTopOfDeck pid 1
      attackModifier attrs attrs (DefenseModifier 1)
      pure h
    _ -> DainIronfootSpirit <$> runMessage msg attrs
