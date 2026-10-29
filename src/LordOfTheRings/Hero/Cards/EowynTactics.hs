module LordOfTheRings.Hero.Cards.EowynTactics (eowynTactics) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype EowynTactics = EowynTactics HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eowynTactics :: HeroCard EowynTactics
eowynTactics = hero EowynTactics Cards.eowynTactics

-- | Action: Raise your threat by 3 to ready Éowyn. She gets +9 attack until the end of
-- the phase. (Limit once per game for the group.)
instance HasAbilities EowynTactics where
  getAbilities (EowynTactics a) =
    [limited (GroupLimit PerGame 1) $ mkAbility a 1 $ ActionAbility (RaiseThreatCost 3)]

instance RunMessage EowynTactics where
  runMessage msg h@(EowynTactics attrs) = case msg of
    -- Setup: Reduce your threat by 3, once the heroes' threat has set it.
    DoStep 3 Setup -> do
      lowerThreatBy attrs attrs.owner 3
      EowynTactics <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      ready attrs
      phaseModifier attrs attrs (AttackModifier 9)
      pure h
    _ -> EowynTactics <$> runMessage msg attrs
