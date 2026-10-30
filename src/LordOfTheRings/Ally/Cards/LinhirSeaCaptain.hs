module LordOfTheRings.Ally.Cards.LinhirSeaCaptain (linhirSeaCaptain) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype LinhirSeaCaptain = LinhirSeaCaptain AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

linhirSeaCaptain :: AllyCard LinhirSeaCaptain
linhirSeaCaptain = ally LinhirSeaCaptain Cards.linhirSeaCaptain

-- | Response: After you play Linhir Sea-captain from your hand, if you paid all of its
-- resource cost from a single hero's resource pool, both that hero and Linhir Sea-captain
-- do not exhaust to quest this round.
instance HasAbilities LinhirSeaCaptain where
  getAbilities (LinhirSeaCaptain a) =
    [mkAbility a 1 $ response $ PaidFromOneHeroWindow After (CardIdTarget a.allyCardId)]

instance RunMessage LinhirSeaCaptain where
  runMessage msg s@(LinhirSeaCaptain a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (take 1 [hid | Window _ (PaidFrom _ (HeroTarget hid : _)) <- windows]) \hid -> do
        roundModifier a hid DoesNotExhaustToCommit
        roundModifier a a DoesNotExhaustToCommit
      pure s
    _ -> LinhirSeaCaptain <$> runMessage msg a
