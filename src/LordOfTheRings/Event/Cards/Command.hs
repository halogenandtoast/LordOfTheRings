{-# LANGUAGE RankNTypes #-}

module LordOfTheRings.Event.Cards.Command (
  commandLeadership,
  commandLore,
  commandSpirit,
  commandTactics,
) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (Card (EncounterCard, PlayerCard), cdCardType, genPlayerCard, cdCost, toCardCode, toCardDef)
import LordOfTheRings.Card.CardDef (CardCost (StaticCost))
import LordOfTheRings.Card.Code (CardCode (..))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Game.Base (gameEntities, gameScenario)
import LordOfTheRings.Attachment.Types (attachmentExhausted)
import LordOfTheRings.Entities (entitiesAttachments)
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Helpers.Campaign (campaignLevel, campaignUpgraded)
import LordOfTheRings.Message.Lifted.UpTo

-- | Command, a scalable boon of the Dream-chaser campaign, one for each sphere.
newtype Command = Command EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

commandLeadership :: EventCard Command
commandLeadership = event Command Cards.commandLeadership

commandLore :: EventCard Command
commandLore = event Command Cards.commandLore

commandSpirit :: EventCard Command
commandSpirit = event Command Cards.commandSpirit

commandTactics :: EventCard Command
commandTactics = event Command Cards.commandTactics

cirdansBlessings :: [CardCode]
cirdansBlessings = map toCardCode [Attachments.cirdansBlessingLeadership, Attachments.cirdansBlessingTactics, Attachments.cirdansBlessingSpirit, Attachments.cirdansBlessingLore]

{- | Action: Add Command to the victory display to choose up to X options, where X is its
level.
Leadership: • A hero gets +2 defense until the end of the round. • An enemy gets -2 defense
until the end of the round. • Ready a character with Círdan's Blessing on it. • Put an ally
with cost X or less in your discard pile into play. Discard it at the end of the phase.
Lore: • Remove X resources from a non-unique enemy. • Add 2 resources to each Círdan's
Blessing. • Add the bottom card of your deck to your hand. • Look at the top 10 cards of the
encounter deck, discard any number of them, and shuffle it.
Spirit: • A hero gets +2 willpower until the end of the round. • Each character with
Círdan's Blessing on it does not exhaust to commit to a quest this round. • Place 2
progress on a non-unique location. • Put Spyglass into play, attached to a hero.
Tactics: • A hero gets +2 attack until the end of the round. • An enemy gets -2 attack
until the end of the round. • Deal 2 damage to an enemy. You may engage it. • Ready an
attachment. You may move it to an eligible character.
-}
instance RunMessage Command where
  runMessage msg e@(Command attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      x <- campaignLevel card
      addToVictoryDisplay card
      heroes <- select AnyHero
      enemies <- select EnemyNotImmuneToPlayerCards
      blessings <- getsGame (filter ((`elem` cirdansBlessings) . toCardCode) . toList . entitiesAttachments . gameEntities)
      let blessed = [h | b <- blessings, AttachedTo h <- [b.placement]]
          option :: Targetable a => Text -> [a] -> (forall n. ReverseQueue n => a -> n ()) -> UpTo
          option key choices act = UpToLabel key $ chooseOrRunOneM pid do
            questionLabeled key
            causedBy attrs
            targets choices act
      options <- case unCardCode (toCardCode card) of
        "DCC201" -> do
          player <- getPlayer pid
          let allies = [pc | PlayerCard pc <- player.discard, cdCardType (toCardDef pc) == AllyType, maybe False (\case StaticCost n -> n <= x; _ -> False) (cdCost $ toCardDef pc)]
          pure
            [ option "command.heroDefense" heroes \h -> roundModifier attrs h (DefenseModifier 2)
            , option "command.enemyDefense" enemies \en -> roundModifier attrs en (DefenseModifier (-2))
            , option "command.readyBlessed" blessed ready
            , option "command.allyFromDiscard" (map (CardIdTarget . (.id)) allies) \case
                CardIdTarget cid -> for_ (find ((== cid) . (.id)) allies) \c -> do
                  removeFromDiscard pid c.id
                  phaseModifier attrs (CardIdTarget c.id) DiscardAtEndOfPhase
                  putAllyIntoPlayUnder pid c
                _ -> pure ()
            ]
        "DCC202" -> do
          player <- getPlayer pid
          top10 <- getsGame (take 10 . (.encounterDeck) . toAttrs . gameScenario)
          nonUnique <- select (not_ UniqueEnemy <> EnemyNotImmuneToPlayerCards)
          pure
            [ option "command.removeResources" nonUnique \en -> removeTokens attrs en Resource x
            , UpToLabel "command.blessingResources" $ for_ blessings \b -> placeResources attrs (toId b) 2
            , UpToLabel "command.bottomCard" $ for_ (lastMay player.deck) \c -> takeFromDeck pid c
            , UpToLabel "command.encounterDeck" do
                chooseUpToM pid "command.discardEncounter" (toTarget attrs) (map EncounterCard top10) 10
                  [ UpToTarget (CardIdTarget c.id) (discardFromEncounterDeck c)
                  | c <- top10
                  ]
                shuffleEncounterDeck
            ]
        "DCC203" -> do
          locations <- select (LocationNotImmuneToPlayerCards <> not_ UniqueLocation)
          spyglassInPlay <- getsGame (any ((`elem` [toCardCode Attachments.spyglass, toCardCode Attachments.spyglassUpgraded]) . toCardCode) . toList . entitiesAttachments . gameEntities)
          upgraded <- campaignUpgraded (toCardCode Attachments.spyglass)
          pure
            $ [ option "command.heroWillpower" heroes \h -> roundModifier attrs h (WillpowerModifier 2)
              , UpToLabel "command.blessedCommit" $ for_ blessed \c -> roundModifier attrs c DoesNotExhaustToCommit
              , option "command.progress" locations \l -> placeProgress attrs l 2
              ]
            <> [ option "command.spyglass" heroes \h -> do
                  glass <- genPlayerCard (if upgraded then Attachments.spyglassUpgraded else Attachments.spyglass) (Just pid)
                  attachOutOfPlay pid glass h
               | not spyglassInPlay
               ]
        _ -> do
          readyable <- getsGame (filter (attachmentExhausted . toAttrs) . toList . entitiesAttachments . gameEntities)
          pure
            [ option "command.heroAttack" heroes \h -> roundModifier attrs h (AttackModifier 2)
            , option "command.enemyAttack" enemies \en -> roundModifier attrs en (AttackModifier (-2))
            , option "command.damage" enemies \en -> do
                dealDamage attrs en 2
                chooseOneM pid do
                  questionLabeled "command.engage"
                  causedBy attrs
                  labeled "command.engageIt" $ engageEnemy pid en
                  labeled "command.dontEngage" $ pure ()
            , option "command.readyAttachment" (map toId readyable) \aid -> do
                ready aid
                for_ (find ((== aid) . toId) readyable) \att -> do
                  let isCharacter = \case
                        HeroTarget _ -> True
                        AllyTarget _ -> True
                        _ -> False
                  hosts <- filter (\h -> isCharacter h && AttachedTo h /= att.placement) <$> cardHosts (toAttrs att).controller (toCardCode att)
                  chooseOneM pid do
                    questionLabeled "command.moveAttachment"
                    causedBy attrs
                    labeled "command.dontMove" $ pure ()
                    targets hosts (moveAttachment aid)
            ]
      chooseUpToM pid "command.choose" (toTarget attrs) [] x options
      pure e
    _ -> Command <$> runMessage msg attrs
