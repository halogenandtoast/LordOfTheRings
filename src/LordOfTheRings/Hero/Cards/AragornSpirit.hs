module LordOfTheRings.Hero.Cards.AragornSpirit (aragornSpirit) where

import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype AragornSpirit = AragornSpirit HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aragornSpirit :: HeroCard AragornSpirit
aragornSpirit = hero AragornSpirit Cards.aragornSpirit

-- | Action: Exhaust Aragorn to choose a location in the staging area. Until the end of the
-- phase, that location gets -X threat, where X is Aragorn's willpower. If this effect
-- reduces the location's threat to 0, place 1 progress on it. (Limit once per round.)
instance HasAbilities AragornSpirit where
  getAbilities (AragornSpirit a) =
    [ limited (PlayerLimit PerRound 1)
        $ restricted a 1 (exists $ LocationInStagingArea <> LocationNotImmuneToPlayerCards)
        $ ActionAbility (exhaustCost a)
    ]

-- | X is counted as the action resolves, as a player card's lasting effect is (FAQ 1.55).
instance RunMessage AragornSpirit where
  runMessage msg h@(AragornSpirit attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      locations <- select (LocationInStagingArea <> LocationNotImmuneToPlayerCards)
      x <- getWillpower (toTarget attrs)
      chooseOrRunOneM pid do
        questionLabeled "chooseLocationInStaging"
        causedBy attrs
        targets locations \lid -> do
          threat <- getLocationThreat lid
          phaseModifier attrs lid (ThreatModifier (negate x))
          when (threat > 0 && threat <= x) $ placeProgress attrs lid 1
      pure h
    _ -> AragornSpirit <$> runMessage msg attrs
