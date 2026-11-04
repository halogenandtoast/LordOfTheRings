module LordOfTheRings.Hero.Cards.LegolasSpirit (legolasSpirit) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype LegolasSpirit = LegolasSpirit HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged, as its card def says.
legolasSpirit :: HeroCard LegolasSpirit
legolasSpirit = hero LegolasSpirit Cards.legolasSpirit

others :: HeroAttrs -> HeroMatcher
others a = ExhaustedHero <> not_ (HeroWithId a.id)

-- | Response: After Legolas commits to a quest, discard a card from your hand to ready
-- another hero. If that hero is Gimli, he gets +1 willpower until the end of the phase.
-- (Limit once per phase.)
instance HasAbilities LegolasSpirit where
  getAbilities (LegolasSpirit a) =
    [ limited (GroupLimit PerPhase 1)
        $ restricted a 1 (exists $ others a)
        $ ResponseAbility (CommittedToQuestWindow After (toTarget a)) (DiscardFromHandCost a.owner 1)
    ]

instance RunMessage LegolasSpirit where
  runMessage msg h@(LegolasSpirit attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select (others attrs)
      chooseOrRunOneM pid do
        questionLabeled "chooseHeroToReady"
        causedBy attrs
        targets heroes \other -> do
          ready other
          gimli <- selectAny (HeroWithId other <> HeroWithTitle "Gimli")
          when gimli $ phaseModifier attrs other (WillpowerModifier 1)
      pure h
    _ -> LegolasSpirit <$> runMessage msg attrs
