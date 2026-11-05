module LordOfTheRings.Quest.Cards.ShrineToSauron (shrineToSauron) where

import LordOfTheRings.Card (CardDef (cdName), HasCardDef, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesAttachments)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTempleOfDoom.Helpers

newtype ShrineToSauron = ShrineToSauron QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

shrineToSauron :: QuestCard ShrineToSauron
shrineToSauron = quest ShrineToSauron Cards.shrineToSauron

instance HasModifiersFor ShrineToSauron where
  getModifiersFor (ShrineToSauron a) = stage2Modifiers a

instance HasAbilities ShrineToSauron where
  getAbilities (ShrineToSauron a) = stage2Forced a

{- | Forced: At the beginning of the quest phase, each player discards the top X cards of
his deck, where X is 1 more than the number of quest stages in the victory display. Each
player examines the cards he controls and the cards in his hand. If any of these cards are
copies of the cards he just discarded from the top of his deck, he must discard those
copies as well. A copy is a card of the same title (Rules Reference, "Copies"), so a hero
or ally sharing a discarded card's title goes too.
-}
instance RunMessage ShrineToSauron where
  runMessage msg q@(ShrineToSauron attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      x <- templeX
      eachPlayer \p -> do
        player <- getPlayer p
        let discarded = map titleOf (take x player.deck)
            copy :: HasCardDef c => c -> Bool
            copy c = titleOf c `elem` discarded
        discardTopOfDeck p x
        heroes <- filterM (fmap copy . getHero) =<< select (HeroControlledBy $ PlayerWithId p)
        allies <- filterM (fmap copy . getAlly) =<< select (AllyControlledBy $ PlayerWithId p)
        attachments <- getsGame (filter (\a -> (toAttrs a).controller == p && copy a) . toList . entitiesAttachments . gameEntities)
        for_ heroes (discard attrs . HeroTarget)
        for_ allies (discard attrs . AllyTarget)
        for_ attachments (discard attrs . toTarget)
        for_ (filter copy player.hand) (discardFromHand p)
      pure q
    _ -> ShrineToSauron <$> runStage2 msg attrs
   where
    titleOf :: HasCardDef c => c -> Text
    titleOf c = let Name t _ = cdName (toCardDef c) in t
