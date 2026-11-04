module LordOfTheRings.Treachery.Cards.TheSavageSouth (theSavageSouth) where

import LordOfTheRings.Card (HasCardDef, cdName, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheSavageSouth = TheSavageSouth TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSavageSouth :: TreacheryCard TheSavageSouth
theSavageSouth = treachery TheSavageSouth Cards.theSavageSouth

title :: HasCardDef a => a -> Text
title c = let Name t _ = cdName (toCardDef c) in t

{- | When Revealed: Each player discards the top 5 cards of his deck. Each player discards
each card he controls that shares a title with any of the 5 cards he just discarded.
Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, return
attacking enemy to the staging area.
-}
instance RunMessage TheSavageSouth where
  runMessage msg t@(TheSavageSouth attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        discarded <- map title . take 5 . (.deck) <$> getPlayer p
        discardTopOfDeck p 5
        heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId p)
        allies <- traverse getAlly =<< select (AllyControlledBy $ PlayerWithId p)
        attachments <- getsGame (filter ((== p) . (.controller) . toAttrs) . toList . (.entities.attachments))
        let sharing :: (HasCardDef a, Targetable a) => [a] -> [Target]
            sharing = map toTarget . filter ((`elem` discarded) . title)
        traverse_ discard $ sharing heroes <> sharing allies <> sharing attachments
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      placeEnemy eid StagingArea
      pure t
    _ -> TheSavageSouth <$> runMessage msg attrs
