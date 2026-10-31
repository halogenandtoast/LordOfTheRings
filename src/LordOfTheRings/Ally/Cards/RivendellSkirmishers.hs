module LordOfTheRings.Ally.Cards.RivendellSkirmishers (rivendellSkirmishers) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype RivendellSkirmishers = RivendellSkirmishers AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign, from The Battle of Carn Dûm.
rivendellSkirmishers :: AllyCard RivendellSkirmishers
rivendellSkirmishers = ally RivendellSkirmishers Cards.rivendellSkirmishers

-- | Response: After Rivendell Skirmishers exhausts to defend an attack from a non-unique
-- enemy, discard each shadow card from that enemy.
instance HasAbilities RivendellSkirmishers where
  getAbilities (RivendellSkirmishers a) =
    [restricted a 1 (exists $ AllyWithId a.id <> ExhaustedAlly) $ response $ DefenderDeclaredWindow After (Just $ toTarget a)]

instance RunMessage RivendellSkirmishers where
  runMessage msg s@(RivendellSkirmishers a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [eid | Window _ (DeclaredAsDefender _ eid) <- windows] \eid -> do
        nonUnique <- selectAny (EnemyWithId eid <> not_ UniqueEnemy)
        when nonUnique do
          e <- getEnemy eid
          for_ e.shadowCards (discardShadowCard eid)
      pure s
    _ -> RivendellSkirmishers <$> runMessage msg a
