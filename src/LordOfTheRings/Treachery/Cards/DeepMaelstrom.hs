module LordOfTheRings.Treachery.Cards.DeepMaelstrom (deepMaelstrom) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DeepMaelstrom = DeepMaelstrom TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

deepMaelstrom :: TreacheryCard DeepMaelstrom
deepMaelstrom = treachery DeepMaelstrom Cards.deepMaelstrom

-- | Cannot be canceled.
instance HasModifiersFor DeepMaelstrom where
  getModifiersFor (DeepMaelstrom a) = modifySelf a [WhenRevealedCannotBeCanceled]

-- | When Revealed: For each location in play that is not grappled, reveal the top card of
-- the encounter deck. If no cards are revealed by this effect, Deep Maelstrom gains Doomed
-- 5.
-- Shadow: Attacking enemy gets +1 attack for each enemy grappled with a location.
instance RunMessage DeepMaelstrom where
  runMessage msg t@(DeepMaelstrom attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      free <- filterM (\l -> not <$> selectAny (EnemyGrappledWith $ LocationWithId l)) =<< select AnyLocation
      if null free then raiseThreatFromDoomed 5 else revealEncounterCards (length free)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      n <- selectCount (EnemyGrappledWith AnyLocation)
      when (n > 0) $ enemyGetsAttack (CardIdSource cid) eid n
      pure t
    _ -> DeepMaelstrom <$> runMessage msg attrs
