module LordOfTheRings.Attachment.Cards.PutOffPursuit (putOffPursuit) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (LocationType))

newtype PutOffPursuit = PutOffPursuit AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to the active location. Limit 1 per location, on its card def.
putOffPursuit :: AttachmentCard PutOffPursuit
putOffPursuit = attachment PutOffPursuit Cards.putOffPursuit & attachTo (AttachToLocation IsActiveLocation)

-- | Attached location gets +2 quest points.
instance HasModifiersFor PutOffPursuit where
  getModifiersFor (PutOffPursuit a) = withHost a \host -> modified a host [QuestPointModifier 2]

-- | Response: After attached location is explored, discard cards from the encounter deck
-- until a location is discarded. Add that location to the staging area to choose a
-- non-unique enemy in the staging area and shuffle it into the encounter deck.
--
-- Answered as the location is explored, while Put Off Pursuit is still on it to read what
-- it discarded. With no location discarded, no enemy is shuffled in.
instance HasAbilities PutOffPursuit where
  getAbilities (PutOffPursuit a) =
    [mkAbility a 1 $ response $ ExploredWindow When lid | AttachedTo (LocationTarget lid) <- [a.placement]]

instance RunMessage PutOffPursuit where
  runMessage msg s@(PutOffPursuit a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discardEncounterCardsUntil a (CardWithType LocationType)
      pure s
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (find (cardMatches $ CardWithType LocationType) cards) \location -> do
        addToStagingFromDiscard location
        doStep 1 msg
      pure s
    DoStep 1 (DiscardedTopOfEncounterDeck (isSource a -> True) _ _) -> do
      enemies <- select (EnemyInStagingArea <> NotEnemy UniqueEnemy <> EnemyNotImmuneToPlayerCards)
      chooseOrRunOneM a.controller do
        questionLabeled "putOffPursuit.shuffle"
        causedBy a
        targets enemies shuffleIntoEncounterDeck
      pure s
    _ -> PutOffPursuit <$> runMessage msg a
