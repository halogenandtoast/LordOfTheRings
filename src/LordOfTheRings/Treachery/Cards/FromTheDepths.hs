module LordOfTheRings.Treachery.Cards.FromTheDepths (fromTheDepths) where

import LordOfTheRings.Card (cdKeywords, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesEnemies)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Keyword (isGrapple)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FromTheDepths = FromTheDepths TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fromTheDepths :: TreacheryCard FromTheDepths
fromTheDepths = treachery FromTheDepths Cards.fromTheDepths

-- | When Revealed: Discard the top 3 cards of the encounter deck. Resolve the Grapple
-- keyword on each enemy discarded by this effect.
-- Shadow: Assign damage among characters you control equal to the number of enemies
-- grappled with locations in the staging area.
instance RunMessage FromTheDepths where
  runMessage msg t@(FromTheDepths attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discardTopOfEncounterDeck attrs 3
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (filter (any isGrapple . cdKeywords . toCardDef) cards) grappleCard
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      staged <- map LocationTarget <$> select LocationInStagingArea
      enemies <- getsGame (toList . entitiesEnemies . gameEntities)
      let n = length [e | e <- enemies, AttachedTo l <- [e.placement], l `elem` staged]
      when (n > 0) $ dealDamageDivided attrs pid n
      pure t
    _ -> FromTheDepths <$> runMessage msg attrs
