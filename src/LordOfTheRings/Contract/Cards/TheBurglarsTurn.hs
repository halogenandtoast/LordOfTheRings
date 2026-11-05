module LordOfTheRings.Contract.Cards.TheBurglarsTurn (theBurglarsTurn) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment (allAttachments)
import LordOfTheRings.Card (Card (PlayerCard), CardCode, CardType (AttachmentType), cdCardTraits, cdName, cdSetIcon, cdSphere, genPlayerCard, toCardCode, toCardDef)
import LordOfTheRings.Contract.CardDefs qualified as Cards
import LordOfTheRings.Contract.Import.Lifted
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Location.Types (LocationAttrs (locationCardId))
import LordOfTheRings.Placement (GuardRelease (AttachWhenFree), Placement (ActiveLocation))
import LordOfTheRings.Sphere (Sphere (..))
import LordOfTheRings.Trait (Trait (Artifact, Boon, Item))

newtype TheBurglarsTurn = TheBurglarsTurn ContractAttrs
  deriving anyclass IsContract
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Side A: You cannot include attachments in your deck (checked as the deck is chosen,
-- 'LordOfTheRings.Decklist').
theBurglarsTurn :: ContractCard TheBurglarsTurn
theBurglarsTurn = contract TheBurglarsTurn Cards.theBurglarsTurn

-- | The loot deck holds 14 cards.
lootDeckSize :: Int
lootDeckSize = 14

title :: CardCode -> Text
title code = let Name t _ = cdName (toCardDef code) in t

{- | Your collection's Item and Artifact attachments, one of each title: every one the game
has, as Helm of Secrecy reads the collection's heroes. Boons, the sagas' Fellowship and
Baggins attachments and Hobbit treasures are left out: Caleb unofficially confirmed that boons and
Fellowship/saga attachments are not meant to be played through this contract (as Alonewolf87
reported in the RingsDB comments on The Burglar's Turn).
-}
collection :: [CardCode]
collection =
  Map.elems
    $ Map.fromListWith
      (\_ earlier -> earlier)
      [ (title code, code)
      | code <- Map.keys allAttachments
      , let def = toCardDef code
      , cardMatches (CardWithType AttachmentType <> CardMatchAny [CardWithTrait Item, CardWithTrait Artifact]) def
      , cdSphere def `notElem` [Just Fellowship, Just Baggins]
      , isNothing (cdSetIcon def)
      , Boon `notMember` cdCardTraits def
      ]

{- | Side B: The active location gets +1 quest point for each attachment attached to it:
the player attachments and the encounter attachments on it, and the player cards it guards
as guarded objectives, as the loot attached to it.
-}
instance HasModifiersFor TheBurglarsTurn where
  getModifiersFor (TheBurglarsTurn a) = when a.flipped do
    actives <- select IsActiveLocation
    for_ actives \lid -> do
      l <- getLocation lid
      attached <- length <$> getAttachmentsOn (LocationTarget lid)
      guarded <- length . filter ((== AttachmentType) . (.cardType) . PlayerCard) <$> playerCardsGuardedBy (locationCardId $ toAttrs l)
      let n = attached + guarded
      when (n > 0) $ modified a (LocationTarget lid) [QuestPointModifier n]

{- | Side B: Forced: After you travel to a location, attach the top card of your loot deck
faceup to that location as a guarded objective. If that attachment has the guarded X
keyword, ignore it.
Forced: When the active location is explored, you may put each guarded attachment on that
location into play at no cost, or add it to your hand.
-}
instance HasAbilities TheBurglarsTurn where
  getAbilities (TheBurglarsTurn a)
    | a.flipped =
        [mkAbility a 1 $ forced $ AnyTraveledWindow After | notNull a.underneath]
          <> [mkAbility a 2 $ forced $ AnyExploredWindow When]
    | otherwise = []

instance RunMessage TheBurglarsTurn where
  runMessage msg c@(TheBurglarsTurn attrs) = case msg of
    -- Side A: Setup: Choose 14 different Item or Artifact attachments from your collection
    -- and shuffle them together. This is your loot deck. Flip this card over and place
    -- your loot deck facedown under it.
    DoStep 3 Setup | not attrs.flipped -> do
      doStep lootDeckSize (WithTarget (toTarget attrs) Setup)
      pure c
    -- each pick is one more title for the loot deck, until it holds 14
    DoStep n (WithTarget (isTarget attrs -> True) Setup) -> do
      let taken = [title (toCardCode card) | card <- attrs.underneath]
          left = [code | code <- collection, title code `notElem` taken]
      if n <= 0 || null left
        then do
          loot <- shuffleM attrs.underneath
          flipOver attrs
          pure $ TheBurglarsTurn $ attrs {contractUnderneath = loot}
        else do
          cards <- traverse (\code -> genPlayerCard (toCardDef code) (Just attrs.owner)) left
          chooseOneM attrs.owner do
            questionLabeledWith "theBurglarsTurn.chooseLoot" [intVar "n" (lootDeckSize - n + 1), intVar "total" lootDeckSize]
            causedBy attrs
            showing (map PlayerCard cards)
            for_ cards \pc -> targeting (CardIdTarget pc.id) do
              placeUnderneath attrs [PlayerCard pc]
              doStep (n - 1) (WithTarget (toTarget attrs) Setup)
          pure c
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      case ([lid | Window After (Traveled lid) <- ws], attrs.underneath) of
        (lid : _, PlayerCard top : rest) -> do
          l <- getLocation lid
          guardPlayerCard (locationCardId $ toAttrs l) AttachWhenFree top
          pure $ TheBurglarsTurn $ attrs {contractUnderneath = rest}
        _ -> pure c
    UseCardAbility pid (isSource attrs -> True) 2 ws -> do
      for_ [lid | Window When (Explored lid) <- ws] \lid -> do
        l <- getLocation lid
        when (l.placement == ActiveLocation) do
          let guard' = locationCardId (toAttrs l)
          loot <- filter ((== AttachmentType) . (.cardType) . PlayerCard) <$> playerCardsGuardedBy guard'
          for_ loot \card -> do
            hosts <- cardHosts pid (toCardCode card)
            chooseOneM pid do
              questionLabeled "theBurglarsTurn.loot"
              causedBy attrs
              showing [PlayerCard card]
              when (notNull hosts) $ labeled "theBurglarsTurn.intoPlay" do
                takeGuardedPlayerCard guard' card
                attachFreedCard pid card
              labeled "theBurglarsTurn.toHand" do
                takeGuardedPlayerCard guard' card
                putInHand card
      pure c
    _ -> TheBurglarsTurn <$> runMessage msg attrs
